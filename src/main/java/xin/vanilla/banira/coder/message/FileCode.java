package xin.vanilla.banira.coder.message;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.ShiroUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.*;

import java.util.ArrayList;
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

        String name = getArg(code, "name");
        String folder = getArg(code, "folder");
        if (StringUtils.isNullOrEmptyEx(name)) name = url.substring(url.replace("\\", "/").lastIndexOf("/") + 1);

        if (StringUtils.isNotNullOrEmpty(jsonPath)) {
            JsonElement json = JsonUtils.parseJson(HttpUtil.get(url));
            if (json != null && !json.isJsonNull()) {
                JsonElement jsonElement = JsonUtils.getJsonElement(json, ShiroUtils.unescape(jsonPath));
                if (jsonElement.isJsonArray()) {
                    for (JsonElement element : jsonElement.getAsJsonArray()) {
                        uploadFile(context, element.getAsString(), name, folder, headerArray);
                    }
                } else if (jsonElement.isJsonPrimitive() && jsonElement.getAsJsonPrimitive().isString()) {
                    uploadFile(context, jsonElement.getAsString(), name, folder, headerArray);
                } else {
                    uploadFile(context, url, name, folder, headerArray);
                }
            }
        } else {
            uploadFile(context, url, name, folder, headerArray);
        }
        context.msg(context.msg().replace(placeholder, ""));
        return "";
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

    private void uploadFile(BaniraCodeContext context, String url, String name, String folder, KeyValue<String, String>[] headerArray) {
        String filePath;
        if (BaniraUtils.allowTraversingFiles(url)) {
            filePath = RandomFileUtils.getRandomFileName(url).orElse(null);
        } else if (BaniraUtils.isLocalFile(url)) {
            filePath = BaniraUtils.convertFileUri(url, headerArray);
        } else if (BaniraUtils.isLocalCacheFile(url, EnumCacheFileType.file)) {
            filePath = BaniraUtils.getCacheAbsolutePath(url, EnumCacheFileType.file);
        } else {
            String fileName = BaniraUtils.downloadFileToCachePath(url, EnumCacheFileType.file, headerArray);
            filePath = BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.file);
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
