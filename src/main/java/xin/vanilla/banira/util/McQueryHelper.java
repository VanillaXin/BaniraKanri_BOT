package xin.vanilla.banira.util;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
public class McQueryHelper {

    private final String serverName;
    private final String serverAddress;
    private int queryPort = 25565;
    @Getter
    private JsonObject serverJson = new JsonObject();
    public static final String ERROR_MSG_LOADING = "LOADING";
    public static final String ERROR_MSG_UNKNOWN_HOST = "UNKNOWN_HOST";
    public static final String ERROR_MSG_CONNECT_FAILED = "FAILED";
    public static final String ERROR_MSG_UNKNOWN_RESPONSE = "UNKNOWN_RESPONSE";

    private McQueryHelper(String name, String address) throws URISyntaxException {
        serverName = name;
        URI uri = new URI("VanillaMCQuery://" + address);
        serverAddress = uri.getHost();
        if (uri.getPort() > 0) {
            queryPort = uri.getPort();
        }
        setError(ERROR_MSG_LOADING);
    }

    @Nullable
    public static McQueryHelper create(String name, String address) {
        try {
            return new McQueryHelper(name, address);
        } catch (URISyntaxException e) {
            LOGGER.error("Failed to create McQueryUtils", e);
            return null;
        }
    }

    public void setDescription(String msg) {
        try {
            serverJson.addProperty("description", msg);
        } catch (Exception e) {
            LOGGER.error("Failed to set description", e);
        }
    }

    public void setError(String msg) {
        try {
            serverJson.addProperty("error", msg);
        } catch (Exception e) {
            LOGGER.error("Failed to set error", e);
        }
    }

    public String error() {
        return JsonUtils.getString(serverJson, "error", "");
    }

    public String serverIp() {
        return serverAddress;
    }

    public int serverPort() {
        return queryPort;
    }

    public String serverAddress() {
        StringBuilder result = new StringBuilder();
        result.append(serverAddress);
        // 若不是默认端口
        if (queryPort != 25565) {
            result.append(":");
            result.append(queryPort);
        }
        return result.toString();
    }

    public String serverName() {
        return serverName;
    }

    public int maxPlayers() {
        JsonObject players = serverJson.getAsJsonObject("players");
        if (players == null) {
            return 0;
        }
        return players.get("max").getAsInt();
    }

    public int onlinePlayers() {
        JsonObject players = serverJson.getAsJsonObject("players");
        if (players == null) {
            return 0;
        }
        return players.get("online").getAsInt();
    }

    public List<String> playerList() {
        List<String> result = new ArrayList<>();
        JsonObject players = serverJson.getAsJsonObject("players");
        if (players == null) {
            return result;
        }
        JsonArray sample = players.getAsJsonArray("sample");
        if (sample == null) {
            return result;
        }
        int pos = 0;
        while (pos < sample.size()) {
            JsonObject entry = sample.get(pos++).getAsJsonObject();
            String username = entry.get("name").getAsString();
            result.add(username);
        }
        Collections.sort(result);
        return result;
    }

    public String playerListString(String separator) {
        List<String> playerList = this.playerList();
        return String.join(separator, playerList);
    }

    public String playerListString() {
        return this.playerListString(", ");
    }

    public String serverVersion() {
        return JsonUtils.getString(serverJson, "version.name", "");
    }

    /**
     * 获取服务器描述的纯文本版本（去除颜色代码）
     */
    public String description() {
        StringBuilder result = new StringBuilder();
        JsonObject descriptionObj = JsonUtils.getJsonObject(serverJson, "description", null);

        if (descriptionObj != null) {
            // 处理包含extra字段的JSON描述
            JsonArray descExtra = JsonUtils.getJsonArray(descriptionObj, "extra", null);
            if (descExtra != null) {
                processExtraArray(descExtra, result, false);
            } else {
                // 处理普通文本描述
                String desc = JsonUtils.getString(descriptionObj, "text", "");
                processPlainDescription(desc, result);
            }
        } else {
            // 处理旧版字符串描述
            String desc = JsonUtils.getString(serverJson, "description", "");
            processPlainDescription(desc, result);
        }

        return result.toString();
    }

    /**
     * 获取带有颜色格式的HTML版本描述
     */
    public String descriptionHtml() {
        StringBuilder result = new StringBuilder();
        JsonObject descriptionObj = JsonUtils.getJsonObject(serverJson, "description", null);

        if (descriptionObj != null) {
            // 处理包含extra字段的JSON描述
            JsonArray descExtra = JsonUtils.getJsonArray(descriptionObj, "extra", null);
            if (descExtra != null) {
                processExtraArray(descExtra, result, true);
            } else {
                // 处理普通文本描述并转换为HTML
                String desc = JsonUtils.getString(descriptionObj, "text", "");
                processHtmlDescription(desc, result);
            }
        } else {
            // 处理旧版字符串描述并转换为HTML
            String desc = JsonUtils.getString(serverJson, "description", "");
            processHtmlDescription(desc, result);
        }

        return result.toString();
    }

    /**
     * 处理extra数组
     */
    private void processExtraArray(JsonArray descExtra, StringBuilder result, boolean isHtml) {
        for (int i = 0; i < descExtra.size(); i++) {
            try {
                JsonObject chunk = descExtra.get(i).getAsJsonObject();
                String text = JsonUtils.getString(chunk, "text", "");

                if (isHtml) {
                    // 为HTML输出添加样式
                    result.append(convertJsonToHtml(chunk, text));
                } else {
                    // 纯文本输出
                    result.append(text);
                }
            } catch (Exception e) {
                LOGGER.error("Failed to process description chunk at index " + i, e);
            }
        }
    }

    /**
     * 处理普通文本描述（去除颜色代码）
     */
    private void processPlainDescription(String desc, StringBuilder result) {
        for (int i = 0; i < desc.length(); i++) {
            char currentChar = desc.charAt(i);
            if (currentChar == '§') {
                // 跳过颜色代码
                i++;
            } else {
                result.append(currentChar);
            }
        }
    }

    /**
     * 处理普通文本描述并转换为HTML
     */
    private void processHtmlDescription(String desc, StringBuilder result) {
        for (int i = 0; i < desc.length(); i++) {
            char currentChar = desc.charAt(i);
            if (currentChar == '§' && i + 1 < desc.length()) {
                // 处理颜色代码
                char colorCode = desc.charAt(++i);
                String style = getStyleFromColorCode(colorCode);

                if (style != null) {
                    result.append(style);
                }
            } else {
                result.append(currentChar);
            }
        }
    }

    /**
     * 将JSON对象转换为带样式的HTML
     */
    private String convertJsonToHtml(JsonObject chunk, String text) {
        StringBuilder styledText = new StringBuilder();
        styledText.append("<span style=\"");

        // 处理颜色
        String color = JsonUtils.getString(chunk, "color", null);
        if (color != null) {
            styledText.append("color:").append(convertMcColorToCss(color)).append(";");
        }

        // 处理粗体
        boolean bold = JsonUtils.getBoolean(chunk, "bold", false);
        if (bold) {
            styledText.append("font-weight:bold;");
        }

        // 处理斜体
        boolean italic = JsonUtils.getBoolean(chunk, "italic", false);
        if (italic) {
            styledText.append("font-style:italic;");
        }

        // 处理下划线
        boolean underlined = JsonUtils.getBoolean(chunk, "underlined", false);
        if (underlined) {
            styledText.append("text-decoration:underline;");
        }

        // 处理删除线
        boolean strikethrough = JsonUtils.getBoolean(chunk, "strikethrough", false);
        if (strikethrough) {
            styledText.append("text-decoration:line-through;");
        }

        // 处理模糊/混淆（Minecraft的obfuscated效果）
        boolean obfuscated = JsonUtils.getBoolean(chunk, "obfuscated", false);
        if (obfuscated) {
            styledText.append("filter: blur(1px);");
        }

        styledText.append("\">").append(text).append("</span>");

        return styledText.toString();
    }

    /**
     * 将Minecraft颜色代码转换为CSS颜色
     */
    private String convertMcColorToCss(String mcColor) {
        // Minecraft颜色名称到十六进制值的映射
        return switch (mcColor) {
            case "dark_red" -> "#AA0000";
            case "red" -> "#FF5555";
            case "gold" -> "#FFAA00";
            case "yellow" -> "#FFFF55";
            case "dark_green" -> "#00AA00";
            case "green" -> "#55FF55";
            case "aqua" -> "#55FFFF";
            case "dark_aqua" -> "#00AAAA";
            case "dark_blue" -> "#0000AA";
            case "blue" -> "#5555FF";
            case "light_purple" -> "#FF55FF";
            case "dark_purple" -> "#AA00AA";
            case "white" -> "#FFFFFF";
            case "gray" -> "#AAAAAA";
            case "dark_gray" -> "#555555";
            case "black" -> "#000000";
            default -> {
                // 如果已经是十六进制颜色，直接返回
                if (mcColor.startsWith("#")) {
                    yield mcColor;
                }
                yield "#FFFFFF";
            }
        };
    }

    /**
     * 从Minecraft颜色代码获取HTML样式
     */
    private String getStyleFromColorCode(char colorCode) {
        return switch (colorCode) {
            case '0' -> "<span style=\"color:#000000\">"; // 黑色
            case '1' -> "<span style=\"color:#0000AA\">"; // 深蓝色
            case '2' -> "<span style=\"color:#00AA00\">"; // 深绿色
            case '3' -> "<span style=\"color:#00AAAA\">"; // 湖蓝色
            case '4' -> "<span style=\"color:#AA0000\">"; // 深红色
            case '5' -> "<span style=\"color:#AA00AA\">"; // 紫色
            case '6' -> "<span style=\"color:#FFAA00\">"; // 金色
            case '7' -> "<span style=\"color:#AAAAAA\">"; // 灰色
            case '8' -> "<span style=\"color:#555555\">"; // 深灰色
            case '9' -> "<span style=\"color:#5555FF\">"; // 蓝色
            case 'a' -> "<span style=\"color:#55FF55\">"; // 绿色
            case 'b' -> "<span style=\"color:#55FFFF\">"; // 天蓝色
            case 'c' -> "<span style=\"color:#FF5555\">"; // 红色
            case 'd' -> "<span style=\"color:#FF55FF\">"; // 粉红色
            case 'e' -> "<span style=\"color:#FFFF55\">"; // 黄色
            case 'f' -> "<span style=\"color:#FFFFFF\">"; // 白色
            case 'k' -> "<span style=\"filter:blur(1px)\">"; // 模糊
            case 'l' -> "<span style=\"font-weight:bold\">"; // 粗体
            case 'm' -> "<span style=\"text-decoration:line-through\">"; // 删除线
            case 'n' -> "<span style=\"text-decoration:underline\">"; // 下划线
            case 'o' -> "<span style=\"font-style:italic\">"; // 斜体
            case 'r' -> "</span>"; // 重置
            default -> null;
        };
    }

    /**
     * See <a href="http://wiki.vg/Protocol">Status Ping</a>
     */
    public void query() {
        Socket socket;
        try {
            socket = new Socket(serverAddress, queryPort);
            socket.setSoTimeout(10000);
        } catch (UnknownHostException e) {
            // 服务器地址有误
            setError(ERROR_MSG_UNKNOWN_HOST);
            return;
        } catch (IllegalArgumentException | IOException e) {
            // 已离线或未启用查询
            setError(ERROR_MSG_CONNECT_FAILED);
            // setDescription("Error: " + e.getLocalizedMessage());
            return;
        }
        OutputStream out;
        InputStream in;
        try {
            out = socket.getOutputStream();
            in = socket.getInputStream();
            // 数据包的总长度
            out.write(6 + serverAddress.length());
            // 数据包ID
            out.write(0);
            // 协议版本
            out.write(4);
            // 服务器地址长度
            out.write(serverAddress.length());
            // 服务器UTF-8地址
            out.write(serverAddress.getBytes());
            // 端口高位字节
            out.write((queryPort & 0xFF00) >> 8);
            // 端口低位字节
            out.write(queryPort & 0x00FF);
            // 下一个状态标志，1: Status, 2: Login, 3: Transfer
            out.write(1);
            // 状态ping的第一个字节
            out.write(0x01);
            // 状态ping的第一个字节
            out.write(0x00);

            // 整个数据包的大小
            int packetLength = readVarInt(in);
            String serverData;
            if (packetLength < 11) {
                LOGGER.warn("{}, {}: packet length is too short: {}\n", serverName, serverAddress, packetLength);
                // 来自服务器的响应无效(服务器可能正在重新启动)
                setError(ERROR_MSG_UNKNOWN_RESPONSE);
                in.close();
                out.close();
                socket.close();
                return;
            }
            // 忽略数据包类型, 只用取一个类型
            in.read();
            // logger.info(String.format("%s, %s: 数据包类型: %d%n", serverName, serverAddress, packetType));
            int jsonLength = readVarInt(in);
            if (jsonLength < 0) {
                in.close();
                out.close();
                socket.close();
                return;
            }
            // 安全起见, 多取10字节
            byte[] buffer = new byte[jsonLength + 10];
            int bytesRead = 0;
            do {
                bytesRead += in.read(buffer, bytesRead, jsonLength - bytesRead);
            } while (bytesRead < jsonLength);
            serverData = new String(buffer, 0, bytesRead);
            serverJson = JsonUtils.GSON.fromJson(serverData, JsonObject.class);
            in.close();
            out.close();
            socket.close();
            setError(null);
        } catch (Exception e) {
            LOGGER.error("Failed to query", e);
        }
    }

    private int readVarInt(InputStream in) {
        int theInt = 0;
        for (int i = 0; i < 6; i++) {
            int theByte;
            try {
                theByte = in.read();
            } catch (IOException e) {
                LOGGER.error("Failed to read var int", e);
                return 0;
            }

            theInt |= (theByte & 0x7F) << (7 * i);
            if (theByte == 0xffffffff) {
                LOGGER.warn("Unusual byte value: {}", theByte);
                return -1;
            }
            if ((theByte & 0x80) != 128) {
                break;
            }
        }
        return theInt;
    }
}
