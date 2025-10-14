package xin.vanilla.banira.coder.message;

import cn.hutool.core.img.FontUtil;
import cn.hutool.core.img.ImgUtil;
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
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * 视频
 */
@Component
public class VideoCode implements MessageCoder {

    private static final Image DEFAULT_COVER = ImgUtil.createImage(" ", FontUtil.createFont(), Color.WHITE, Color.BLACK, BufferedImage.TYPE_4BYTE_ABGR);

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "pic/reimu.mp4" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "视频";
    }

    @Override
    public String getDesc() {
        return "短视频消息";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "video"
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

        String cover = getArg(code, "cover");
        if (StringUtils.isNullOrEmptyEx(cover)) {
            Image image = ImgUtil.copyImage(DEFAULT_COVER, BufferedImage.TYPE_4BYTE_ABGR);
            if (context.keywordRecord() != null) {
                image = ImgUtil.pressText(image, context.keywordRecord().getKeyword(), Color.BLACK, FontUtil.createFont(), 0, 0, 1f);
            } else {
                image = ImgUtil.pressText(image, url, Color.BLACK, FontUtil.createFont(), 0, 0, 1f);
            }
            cover = ImgUtil.toBase64(image, "JPG");
        }
        MsgUtils builder = MsgUtils.builder();
        if (StringUtils.isNotNullOrEmpty(jsonPath)) {
            JsonElement json = JsonUtils.parseJson(HttpUtil.get(url));
            if (json != null && !json.isJsonNull()) {
                JsonElement jsonElement = JsonUtils.getJsonElement(json, ShiroUtils.unescape(jsonPath));
                if (jsonElement.isJsonArray()) {
                    for (JsonElement element : jsonElement.getAsJsonArray()) {
                        builder.video(downloadVideo(element.getAsString(), headerArray), cover);
                    }
                } else if (jsonElement.isJsonPrimitive() && jsonElement.getAsJsonPrimitive().isString()) {
                    builder.video(downloadVideo(jsonElement.getAsString(), headerArray), cover);
                } else {
                    builder.video(downloadVideo(url, headerArray), cover);
                }
                context.msg(context.msg().replace(placeholder, replaceResult(code, builder.build())));
                return builder.build();
            }
        }
        builder.video(downloadVideo(url, headerArray), cover);
        context.msg(context.msg().replace(placeholder, replaceResult(code, builder.build())));
        return builder.build();
    }


    public static String build(String url) {
        Image image = ImgUtil.copyImage(DEFAULT_COVER, BufferedImage.TYPE_4BYTE_ABGR);
        image = ImgUtil.pressText(image, url, Color.BLACK, FontUtil.createFont(), 0, 0, 1f);
        return build(url, ImgUtil.toBase64(image, "JPG"));
    }

    public static String build(String url, String cover) {
        return CODE_START + CollectionUtils.getRandomElement(types)
                + ARG_SEPARATOR + "url" + VAL_SEPARATOR + url
                + ARG_SEPARATOR + "cover" + VAL_SEPARATOR + cover
                + CODE_END;
    }

    private static String downloadVideo(String url, KeyValue<String, String>[] headerArray) {
        if (BaniraUtils.isLocalFile(url)) {
            return new File(url).getAbsolutePath();
        } else if (BaniraUtils.isLocalCacheFile(url, EnumCacheFileType.video)) {
            return BaniraUtils.getCacheAbsolutePath(url, EnumCacheFileType.video);
        } else {
            String fileName = BaniraUtils.downloadFileToCachePath(url, EnumCacheFileType.video, headerArray);
            return BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.video);
        }
    }

}
