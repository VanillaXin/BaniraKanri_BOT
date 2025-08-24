package xin.vanilla.banira.util;

import jakarta.annotation.Nullable;

import java.net.IDN;
import java.util.regex.Pattern;

public final class NetAddressUtils {

    private NetAddressUtils() {
    }

    // IPv4 正则
    private static final Pattern IPV4_PATTERN = Pattern.compile(
            "^(25[0-5]|2[0-4]\\d|1\\d{2}|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d{2}|[1-9]?\\d)){3}$"
    );

    // IPv6 正则（压缩形式也支持）
    private static final Pattern IPV6_PATTERN = Pattern.compile(
            "^(?:[\\da-fA-F]{1,4}:){7}[\\da-fA-F]{1,4}$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,7}:$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,6}:[\\da-fA-F]{1,4}$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,5}(?::[\\da-fA-F]{1,4}){1,2}$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,4}(?::[\\da-fA-F]{1,4}){1,3}$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,3}(?::[\\da-fA-F]{1,4}){1,4}$" +
                    "|^(?:[\\da-fA-F]{1,4}:){1,2}(?::[\\da-fA-F]{1,4}){1,5}$" +
                    "|^[\\da-fA-F]{1,4}:(?:(?::[\\da-fA-F]{1,4}){1,6})$" +
                    "|^:(?:(?::[\\da-fA-F]{1,4}){1,7}|:)$"
    );

    // 域名正则（已转为 ASCII/Punycode）
    private static final Pattern DOMAIN_PATTERN = Pattern.compile(
            "^(?=.{1,253}$)(?!-)(?:[a-zA-Z0-9-]{1,63}\\.)+[a-zA-Z]{2,63}$"
    );

    public static boolean isIPv4(String input) {
        return input != null && IPV4_PATTERN.matcher(input).matches();
    }

    public static boolean isIPv6(String input) {
        return input != null && IPV6_PATTERN.matcher(input).matches();
    }

    public static boolean isDomain(String input) {
        if (input == null || input.isEmpty()) return false;
        try {
            // 转换为 Punycode 进行匹配
            String ascii = IDN.toASCII(input, IDN.ALLOW_UNASSIGNED);
            return DOMAIN_PATTERN.matcher(ascii).matches();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    public static boolean isValidNetAddress(String input) {
        return getType(input) != AddressType.INVALID;
    }

    public enum AddressType {
        IPv4, IPv6, DOMAIN, INVALID
    }

    /**
     * 获取地址类型
     */
    public static AddressType getType(String input) {
        if (isIPv4(input)) return AddressType.IPv4;
        if (isIPv6(input)) return AddressType.IPv6;
        if (isDomain(input)) return AddressType.DOMAIN;
        return AddressType.INVALID;
    }

    private static final int DEFAULT_PORT = 25565;

    /**
     * 从 tokens 中寻找第一个可能的远程地址，并解析 host/host + port。
     *
     * @param tokens    输入分词（split 后的字符串数组）
     * @param fromIndex 开始查找的下标（通常是 1，因为 0 是命令字）
     */
    @Nullable
    public static NetAddress findAddressAndPort(String[] tokens, int fromIndex) {
        if (tokens == null || tokens.length <= fromIndex) {
            return null;
        }

        for (int i = fromIndex; i < tokens.length; i++) {
            String t = tokens[i];
            if (t == null || t.isEmpty()) continue;

            // host:port
            if (t.contains(":")) {
                String[] parts = t.split(":", 2);
                String ip = parts[0];
                if (NetAddressUtils.isValidNetAddress(ip)) {
                    int port = DEFAULT_PORT;
                    if (parts.length == 2 && isValidPort(parts[1])) {
                        port = StringUtils.toInt(parts[1]);
                    }
                    return new NetAddress(ip, port, i);
                }
            }

            // host port
            if (NetAddressUtils.isValidNetAddress(t)) {
                int port = DEFAULT_PORT;
                // 判断下一个 token 是否是端口号
                if (i + 1 < tokens.length && isValidPort(tokens[i + 1])) {
                    port = StringUtils.toInt(tokens[i + 1]);
                }
                return new NetAddress(t, port, i);
            }
        }
        return null;
    }

    public static boolean isValidPort(String s) {
        int port = StringUtils.toInt(s);
        return port > 0 && port < 65536;
    }

    public record NetAddress(
            String host,
            int port,
            int index
    ) {

        @Override
        public String toString() {
            return host + ":" + port;
        }

    }

}
