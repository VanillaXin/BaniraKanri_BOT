package xin.vanilla.banira.coder.message;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.*;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * 语音
 */
@Component
public class VoiceCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "music/Old Memory.mp3" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "语音";
    }

    @Override
    public String getDesc() {
        return "语音消息";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "voice"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        JsonObject data = code.getData();
        if (data == null) return fail(context, code, placeholder);

        String url = getValue(context, code, "url");
        if (StringUtils.isNullOrEmptyEx(url)) return fail(context, code, placeholder);

        String jsonPath = getArg(code, "path", "jsonpath");

        String headers = ShiroUtils.unescape(getArg(code, "headers", "header"));
        KeyValue<String, String>[] headerArray = null;
        if (StringUtils.isNotNullOrEmpty(headers)) {
            JsonObject jsonObject = JsonUtils.parseJsonObject(headers);
            if (jsonObject != null && !jsonObject.isJsonNull()) {
                List<KeyValue<String, String>> headerList = new ArrayList<>();
                for (String key : jsonObject.keySet()) {
                    headerList.add(new KeyValue<>(key, JsonUtils.getString(jsonObject, key)));
                }
                headerArray = headerList.toArray(new KeyValue[0]);
            }
        }

        MsgUtils builder = MsgUtils.builder();
        if (StringUtils.isNotNullOrEmpty(jsonPath)) {
            JsonElement json = JsonUtils.parseJson(HttpUtil.get(url));
            if (json != null && !json.isJsonNull()) {
                JsonElement jsonElement = JsonUtils.getJsonElement(json, ShiroUtils.unescape(jsonPath));
                if (jsonElement.isJsonArray()) {
                    for (JsonElement element : jsonElement.getAsJsonArray()) {
                        builder.voice(downloadVoice(element.getAsString(), headerArray));
                    }
                } else if (jsonElement.isJsonPrimitive() && jsonElement.getAsJsonPrimitive().isString()) {
                    builder.voice(downloadVoice(jsonElement.getAsString(), headerArray));
                } else {
                    builder.voice(downloadVoice(url, headerArray));
                }
                context.msg(context.msg().replace(placeholder, replaceResult(code, builder.build())));
                return builder.build();
            }
        }
        builder.voice(downloadVoice(url, headerArray));
        context.msg(context.msg().replace(placeholder, replaceResult(code, builder.build())));
        return builder.build();
    }

    public static String build(String url) {
        return CODE_START + CollectionUtils.getRandomElement(types)
                + ARG_SEPARATOR + "url" + VAL_SEPARATOR + url
                + CODE_END;
    }

    private static String downloadVoice(String url, KeyValue<String, String>[] headerArray) {
        if (BaniraUtils.allowTraversingFiles(url)) {
            return RandomFileUtils.getRandomFileName(url).orElse(null);
        } else if (BaniraUtils.isLocalFile(url)) {
            return new File(url).getAbsolutePath();
        } else if (BaniraUtils.isLocalCacheFile(url, EnumCacheFileType.voice)) {
            return BaniraUtils.getCacheAbsolutePath(url, EnumCacheFileType.voice);
        } else {
            String fileName = BaniraUtils.downloadFileToCachePath(url, EnumCacheFileType.voice, headerArray);
            return BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.voice);
        }
    }

}
