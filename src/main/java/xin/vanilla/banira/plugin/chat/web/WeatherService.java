package xin.vanilla.banira.plugin.chat.web;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.http.HttpResponse;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;

@Slf4j
@Component
public class WeatherService {

    @Nonnull
    public String currentWeather(@Nonnull String location) {
        String normalized = normalizeLocation(location);
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return "缺少城市名";
        }
        String url = "https://wttr.in/" + URLEncoder.encode(normalized, StandardCharsets.UTF_8)
                + "?format=j1&lang=zh";
        try {
            HttpResponse response = HttpUtils.get(url)
                    .header("User-Agent", "curl/8.0")
                    .timeout(Duration.ofSeconds(8))
                    .execute();
            if (response == null || response.statusCode() < 200 || response.statusCode() >= 300) {
                LOGGER.debug("weather query failed status={}", response != null ? response.statusCode() : null);
                return "天气查询失败";
            }
            return formatWttrJson(normalized, response.getBodyAsString());
        } catch (Exception e) {
            LOGGER.debug("weather query failed location={}", normalized, e);
            return "天气查询失败";
        }
    }

    @Nonnull
    static String formatWttrJson(@Nonnull String location, @Nonnull String json) {
        JsonObject root = JsonParser.parseString(json).getAsJsonObject();
        JsonObject current = firstObject(root, "current_condition");
        JsonObject today = firstObject(root, "weather");
        String desc = firstValue(current, "weatherDesc", "value");
        String temp = string(current, "temp_C");
        String feels = string(current, "FeelsLikeC");
        String humidity = string(current, "humidity");
        String wind = string(current, "windspeedKmph");
        String min = string(today, "mintempC");
        String max = string(today, "maxtempC");
        StringBuilder builder = new StringBuilder(location).append("现在");
        if (StringUtils.isNotNullOrEmpty(desc)) {
            builder.append(desc);
        }
        if (StringUtils.isNotNullOrEmpty(temp)) {
            builder.append("，").append(temp).append("℃");
        }
        if (StringUtils.isNotNullOrEmpty(feels)) {
            builder.append("，体感").append(feels).append("℃");
        }
        if (StringUtils.isNotNullOrEmpty(min) || StringUtils.isNotNullOrEmpty(max)) {
            builder.append("\n今天");
            if (StringUtils.isNotNullOrEmpty(min)) {
                builder.append("最低").append(min).append("℃");
            }
            if (StringUtils.isNotNullOrEmpty(max)) {
                builder.append("，最高").append(max).append("℃");
            }
        }
        if (StringUtils.isNotNullOrEmpty(humidity) || StringUtils.isNotNullOrEmpty(wind)) {
            builder.append("\n");
            if (StringUtils.isNotNullOrEmpty(humidity)) {
                builder.append("湿度").append(humidity).append("%");
            }
            if (StringUtils.isNotNullOrEmpty(wind)) {
                if (StringUtils.isNotNullOrEmpty(humidity)) {
                    builder.append("，");
                }
                builder.append("风速").append(wind).append("km/h");
            }
        }
        return builder.toString();
    }

    @Nonnull
    private static String normalizeLocation(@Nonnull String location) {
        return location
                .replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("@\\S{1,32}", " ")
                .replaceAll("(今天|明天|后天|天气|查一下|查询|帮我|看看|看一下)", " ")
                .replaceAll("\\s+", " ")
                .trim();
    }

    private static JsonObject firstObject(@Nonnull JsonObject root, @Nonnull String key) {
        JsonArray array = root.has(key) && root.get(key).isJsonArray() ? root.getAsJsonArray(key) : new JsonArray();
        return !array.isEmpty() && array.get(0).isJsonObject() ? array.get(0).getAsJsonObject() : new JsonObject();
    }

    @Nonnull
    private static String firstValue(@Nonnull JsonObject root, @Nonnull String arrayKey, @Nonnull String valueKey) {
        JsonArray array = root.has(arrayKey) && root.get(arrayKey).isJsonArray() ? root.getAsJsonArray(arrayKey) : new JsonArray();
        if (array.isEmpty() || !array.get(0).isJsonObject()) {
            return "";
        }
        return string(array.get(0).getAsJsonObject(), valueKey);
    }

    @Nonnull
    private static String string(@Nonnull JsonObject object, @Nonnull String key) {
        return object.has(key) && !object.get(key).isJsonNull() ? object.get(key).getAsString().trim() : "";
    }
}
