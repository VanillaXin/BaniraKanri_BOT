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
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;
import java.util.Set;

/**
 * 视频
 */
@Component
public class VideoCode implements MessageCoder {

    private static final String DEFAULT_COVER = ImgUtil.toBase64(
            ImgUtil.createImage(" ", FontUtil.createFont(), Color.BLACK, Color.BLACK, BufferedImage.TYPE_4BYTE_ABGR)
            , "JPG"
    );

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
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        JsonObject data = code.getData();
        if (data == null) return fail(context, code, placeholder);
        String jsonPath = JsonUtils.getString(data, "path", "");
        if (StringUtils.isNullOrEmptyEx(jsonPath)) JsonUtils.getString(data, "jsonpath", "");
        String url = JsonUtils.getString(data, "url", JsonUtils.getString(data, "value", ""));
        if (StringUtils.isNullOrEmptyEx(url)) return fail(context, code, placeholder);
        String cover = JsonUtils.getString(data, "cover", DEFAULT_COVER);
        if (StringUtils.isNotNullOrEmpty(jsonPath)) {
            JsonElement json = JsonUtils.parseJson(HttpUtil.get(url));
            if (json != null && !json.isJsonNull()) {
                JsonElement jsonElement = JsonUtils.getJsonElement(json, ShiroUtils.unescape(jsonPath));
                MsgUtils builder = MsgUtils.builder();
                if (jsonElement.isJsonArray()) {
                    for (JsonElement element : jsonElement.getAsJsonArray()) {
                        builder.video(downloadVideo(element.getAsString()), cover);
                    }
                } else if (jsonElement.isJsonPrimitive() && jsonElement.getAsJsonPrimitive().isString()) {
                    builder.video(downloadVideo(jsonElement.getAsString()), cover);
                } else {
                    builder.video(downloadVideo(url), cover);
                }
                return context.msg(context.msg().replace(placeholder, builder.build()));
            }
        }
        MsgUtils builder = MsgUtils.builder().video(downloadVideo(url), cover);
        return context.msg(context.msg().replace(placeholder, builder.build()));
    }


    public static String build(String url) {
        return build(url, DEFAULT_COVER);
    }

    public static String build(String url, String cover) {
        return CODE_START + CollectionUtils.getRandomElement(types)
                + ARG_SEPARATOR + "url" + VAL_SEPARATOR + url
                + ARG_SEPARATOR + "cover" + VAL_SEPARATOR + cover
                + CODE_END;
    }

    private static String downloadVideo(String url) {
        if (BaniraUtils.isLocalFile(url)) {
            return new File(url).getAbsolutePath();
        } else if (BaniraUtils.isLocalCacheFile(url, EnumCacheFileType.video)) {
            return BaniraUtils.getCacheAbsolutePath(url, EnumCacheFileType.video);
        } else {
            String fileName = BaniraUtils.downloadFileToCachePath(url, EnumCacheFileType.video);
            return BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.video);
        }
    }

}
