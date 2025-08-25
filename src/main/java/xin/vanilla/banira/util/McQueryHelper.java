package xin.vanilla.banira.util;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;

import java.io.*;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

@Slf4j
public class McQueryHelper {

    private final String serverName;
    private final String serverAddress;
    private int queryPort = 25565;
    @Getter
    private JsonObject serverJson = new JsonObject();
    @Getter
    private long ping = -1;

    public static final String ERROR_MSG_LOADING = "LOADING";
    public static final String ERROR_MSG_UNKNOWN_HOST = "UNKNOWN_HOST";
    public static final String ERROR_MSG_CONNECT_FAILED = "FAILED";
    public static final String ERROR_MSG_UNKNOWN_RESPONSE = "UNKNOWN_RESPONSE";
    public static final String ERROR_MSG_PING_FAILED = "PING_FAILED";

    private static final int PACKET_TYPE_HANDSHAKE = 0;
    private static final int PACKET_TYPE_STATUS = 0;
    private static final int PROTOCOL_VERSION = 4;
    private static final int SOCKET_TIMEOUT = 10000;
    private static final AtomicLong PING_ID = new AtomicLong(1);

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
            if (msg == null) {
                serverJson.remove("error");
            } else {
                serverJson.addProperty("error", msg);
            }
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

    public List<KeyValue<String, String>> playerList() {
        List<KeyValue<String, String>> result = new ArrayList<>();
        JsonObject players = serverJson.getAsJsonObject("players");
        if (players == null) {
            return result;
        }
        JsonArray sample = players.getAsJsonArray("sample");
        if (sample == null) {
            return result;
        }
        for (int i = 0; i < sample.size(); i++) {
            JsonObject entry = sample.get(i).getAsJsonObject();
            String username = JsonUtils.getString(entry, "name", "");
            String uuid = JsonUtils.getString(entry, "id", "");
            result.add(new KeyValue<>(username, uuid));
        }
        result.sort(Comparator.comparing(KeyValue::getKey));
        return result;
    }

    public String playerListString(String separator) {
        List<KeyValue<String, String>> playerList = this.playerList();
        return String.join(separator, playerList.stream().map(KeyValue::getKey).toList());
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
     * 查询服务器状态信息
     * See <a href="http://wiki.vg/Protocol">Status Ping</a>
     */
    public void query() {
        try (Socket socket = new Socket(serverAddress, queryPort)) {
            socket.setSoTimeout(SOCKET_TIMEOUT);
            try (OutputStream out = socket.getOutputStream();
                 InputStream in = socket.getInputStream()) {

                // 发送握手包
                sendHandshakePacket(out);

                // 发送状态请求包
                sendStatusRequestPacket(out);

                // 读取状态响应
                readStatusResponse(in);

                // 尝试获取ping值
                measurePing(socket, out, in);

                setError(null);
            }
        } catch (UnknownHostException e) {
            // 服务器地址有误
            setError(ERROR_MSG_UNKNOWN_HOST);
        } catch (IllegalArgumentException | IOException e) {
            // 已离线或未启用查询
            setError(ERROR_MSG_CONNECT_FAILED);
        } catch (Exception e) {
            LOGGER.error("Failed to query server {}", serverAddress, e);
            setError(ERROR_MSG_UNKNOWN_RESPONSE);
        }
    }

    /**
     * 发送握手包
     */
    private void sendHandshakePacket(OutputStream out) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        DataOutputStream data = new DataOutputStream(buffer);

        // 数据包ID (Handshake)
        writeVarInt(data, PACKET_TYPE_HANDSHAKE);
        // 协议版本
        writeVarInt(data, PROTOCOL_VERSION);
        // 服务器地址长度和地址
        writeString(data, serverAddress);
        // 服务器端口
        data.writeShort(queryPort);
        // 下一步状态 (1 for status)
        writeVarInt(data, 1);

        // 发送数据包
        sendPacket(out, buffer.toByteArray());
    }

    /**
     * 发送状态请求包
     */
    private void sendStatusRequestPacket(OutputStream out) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        DataOutputStream data = new DataOutputStream(buffer);

        // 数据包ID (Status Request)
        writeVarInt(data, PACKET_TYPE_STATUS);

        // 发送数据包
        sendPacket(out, buffer.toByteArray());
    }

    /**
     * 读取状态响应
     */
    private void readStatusResponse(InputStream in) throws IOException {
        // 读取数据包长度
        int packetLength = readVarInt(in);

        if (packetLength < 1) {
            throw new IOException("Invalid packet length: " + packetLength);
        }

        // 读取数据包ID
        int packetId = readVarInt(in);
        if (packetId != 0) {
            throw new IOException("Unexpected packet ID: " + packetId);
        }

        // 读取JSON数据长度
        int jsonLength = readVarInt(in);
        if (jsonLength < 1) {
            throw new IOException("Invalid JSON length: " + jsonLength);
        }

        // 读取JSON数据
        byte[] jsonData = new byte[jsonLength];
        int bytesRead = 0;
        while (bytesRead < jsonLength) {
            int read = in.read(jsonData, bytesRead, jsonLength - bytesRead);
            if (read == -1) {
                throw new IOException("Unexpected end of stream while reading JSON");
            }
            bytesRead += read;
        }

        String serverData = new String(jsonData);
        serverJson = JsonUtils.GSON.fromJson(serverData, JsonObject.class);
    }

    /**
     * 测量服务器ping值
     */
    private void measurePing(Socket socket, OutputStream out, InputStream in) {
        try {
            long pingId = PING_ID.getAndIncrement();
            long startTime = System.currentTimeMillis();

            // 发送ping请求
            sendPingRequest(out, pingId);

            // 读取ping响应
            if (readPingResponse(in, pingId)) {
                long endTime = System.currentTimeMillis();
                ping = endTime - startTime;
                serverJson.addProperty("ping", ping);
            } else {
                setError(ERROR_MSG_PING_FAILED);
            }
        } catch (Exception e) {
            LOGGER.warn("Failed to measure ping for server {}", serverAddress, e);
            // Ping失败不影响主要状态查询
        }
    }

    /**
     * 发送ping请求
     */
    private void sendPingRequest(OutputStream out, long pingId) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        DataOutputStream data = new DataOutputStream(buffer);

        // 数据包ID (Ping)
        writeVarInt(data, 1);
        // Ping负载
        data.writeLong(pingId);

        // 发送数据包
        sendPacket(out, buffer.toByteArray());
    }

    /**
     * 读取ping响应
     */
    private boolean readPingResponse(InputStream in, long expectedPingId) throws IOException {
        // 设置读取超时时间为2秒
        int timeout = 2000;
        long startTime = System.currentTimeMillis();

        while (System.currentTimeMillis() - startTime < timeout) {
            if (in.available() > 0) {
                // 读取数据包长度
                int packetLength = readVarInt(in);

                // 读取数据包ID
                int packetId = readVarInt(in);

                if (packetId == 1) {
                    // 读取ping响应负载
                    long pingId = readLong(in);
                    return pingId == expectedPingId;
                }
            }

            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }

        return false;
    }

    /**
     * 发送数据包（添加长度前缀）
     */
    private void sendPacket(OutputStream out, byte[] data) throws IOException {
        ByteArrayOutputStream packetBuffer = new ByteArrayOutputStream();
        writeVarInt(packetBuffer, data.length);
        packetBuffer.write(data);
        out.write(packetBuffer.toByteArray());
        out.flush();
    }

    /**
     * 读取VarInt
     */
    private int readVarInt(InputStream in) throws IOException {
        int value = 0;
        int length = 0;
        byte currentByte;

        do {
            currentByte = (byte) in.read();
            value |= (currentByte & 0x7F) << (length * 7);
            length++;

            if (length > 5) {
                throw new IOException("VarInt too long");
            }
        } while ((currentByte & 0x80) == 0x80);

        return value;
    }

    /**
     * 写入VarInt
     */
    private void writeVarInt(OutputStream out, int value) throws IOException {
        do {
            byte temp = (byte) (value & 0x7F);
            value >>>= 7;
            if (value != 0) {
                temp |= 0x80;
            }
            out.write(temp);
        } while (value != 0);
    }

    /**
     * 写入字符串
     */
    private void writeString(DataOutputStream out, String value) throws IOException {
        byte[] bytes = value.getBytes();
        writeVarInt(out, bytes.length);
        out.write(bytes);
    }

    /**
     * 读取long值
     */
    private long readLong(InputStream in) throws IOException {
        byte[] bytes = new byte[8];
        int bytesRead = in.read(bytes);
        if (bytesRead != 8) {
            throw new IOException("Failed to read long value");
        }

        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        buffer.order(ByteOrder.BIG_ENDIAN);
        return buffer.getLong();
    }
}
