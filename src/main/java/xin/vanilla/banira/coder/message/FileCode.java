package xin.vanilla.banira.coder.message;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.io.File;
import java.util.List;
import java.util.Set;

/**
 * 文件
 */
@Component
public class FileCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "pic/reimu.mp4" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "文件";
    }

    @Override
    public String getDesc() {
        return "上传文件";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "file"
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
        String url = JsonUtils.getString(data, "url", "");
        if (StringUtils.isNullOrEmptyEx(url)) url = JsonUtils.getString(data, "value", "");
        if (StringUtils.isNullOrEmptyEx(url)) return fail(context, code, placeholder);
        String name = JsonUtils.getString(data, "name", "");
        String folder = JsonUtils.getString(data, "folder", "");
        if (StringUtils.isNullOrEmptyEx(name)) name = url.substring(url.replace("\\", "/").lastIndexOf("/") + 1);

        if (StringUtils.isNotNullOrEmpty(jsonPath)) {
            JsonElement json = JsonUtils.parseJson(HttpUtil.get(url));
            if (json != null && !json.isJsonNull()) {
                JsonElement jsonElement = JsonUtils.getJsonElement(json, jsonPath);
                if (jsonElement.isJsonArray()) {
                    for (JsonElement element : jsonElement.getAsJsonArray()) {
                        uploadFile(context, element.getAsString(), name, folder);
                    }
                } else if (jsonElement.isJsonPrimitive() && jsonElement.getAsJsonPrimitive().isString()) {
                    uploadFile(context, jsonElement.getAsString(), name, folder);
                } else {
                    uploadFile(context, url, name, folder);
                }
            }
        } else {
            uploadFile(context, url, name, folder);
        }
        return context.msg(context.msg().replace(placeholder, ""));
    }

    public static String build(String url) {
        return build(url, url.replace("\\", "/").substring(url.lastIndexOf("/") + 1));
    }

    public static String build(String url, String cover) {
        return CODE_START + CollectionUtils.getRandomElement(types)
                + ARG_SEPARATOR + "url" + VAL_SEPARATOR + url
                + ARG_SEPARATOR + "name" + VAL_SEPARATOR + cover
                + CODE_END;
    }

    private void uploadFile(BaniraCodeContext context, String url, String name, String folder) {
        String filePath = null;
        File file = new File(url);
        if (file.exists()) {
            filePath = file.getAbsolutePath();
        } else {
            String fileName = BaniraUtils.downloadFileToCachePath(url);
            if (StringUtils.isNotNullOrEmpty(fileName)) {
                filePath = new File("cache/file/", fileName).getAbsolutePath();
            }
        }
        if (StringUtils.isNotNullOrEmpty(filePath)) {
            name = name.replaceAll("[\\\\/:*?\"<>|]", "");
            if (BaniraUtils.isGroupIdValid(context.group())) {
                context.bot().uploadGroupFile(context.group(), filePath, name, folder);
            } else {
                context.bot().uploadPrivateFile(context.sender(), filePath, name);
            }
        }
    }

}
