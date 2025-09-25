package xin.vanilla.banira.util;

import xin.vanilla.banira.domain.KeyValue;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public final class PlantCipher {

    private PlantCipher() {
    }

    public static final List<KeyValue<String, String>> LOCATOR = List.of(
            new KeyValue<>("阁下请喝", "茶"),
            new KeyValue<>("阁下请用", "茶"),
            new KeyValue<>("阁下请品", "茶")
    );
    private static final String SALT = "BaniraKanri";
    private static final int TOKEN_CHAR_LENGTH = 1;
    private static final int GCM_TAG_BITS = 128;
    private static final int GCM_IV_BYTES = 12; // 96 bits
    private static final String AES_ALGO = "AES/GCM/NoPadding";
    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    private static final List<String> BASE_PLANTS = BaniraUtils.mutableListOf(
            "艾", "桉", "薁", "芭", "菝", "柏", "稗", "蒡", "苞", "枹", "荸",
            "秕", "粃", "荜", "蓽", "蓖", "薜", "篦", "萹", "藨", "槟", "檳",
            "菠", "瓟", "㼎", "檗", "参", "參", "箣", "岑", "梣", "檫", "菖",
            "陈", "陳", "柽", "檉", "橙", "黐", "椆", "楮", "椿", "莼", "蒓",
            "茨", "苁", "葱", "楤", "蔥", "蓯", "欓", "菪", "稲", "稻", "荻",
            "蒂", "棣", "苳", "豆", "芏", "杜", "椴", "莪", "萼", "榧", "枫",
            "楓", "稃", "芙", "茯", "莩", "菔", "柑", "橄", "藁", "茖", "葛",
            "茛", "枸", "构", "構", "菰", "菇", "瓜", "栝", "桄", "瑰", "桂",
            "桧", "蔊", "笐", "蒿", "诃", "訶", "禾", "荷", "蕻", "葫", "瑚",
            "槲", "瓠", "桦", "樺", "槐", "荁", "茴", "荟", "蕙", "薈", "檜",
            "藿", "芨", "棘", "蒺", "蕺", "荠", "蓟", "薊", "檵", "菅", "樫",
            "茳", "姜", "豇", "橿", "艽", "椒", "蕉", "藠", "芥", "堇", "槿",
            "荩", "藎", "荆", "荊", "菁", "韭", "桕", "桔", "菊", "橘", "椇",
            "蒟", "榉", "櫸", "苣", "蕨", "菌", "咖", "楷", "栲", "柯", "蔻",
            "葵", "莱", "萊", "梾", "棶", "兰", "蘭", "榄", "欖", "榔", "莨",
            "簩", "艻", "蘽", "梨", "藜", "苈", "荔", "栎", "莉", "栗", "藶",
            "櫟", "莲", "蓮", "蔹", "蘞", "楝", "椋", "蓼", "蔺", "橉", "藺",
            "苓", "柃", "菱", "榴", "柳", "蒌", "蔞", "芦", "栌", "蘆", "櫨",
            "蕗", "栾", "欒", "萝", "椤", "蘿", "欏", "榈", "櫚", "葎", "荬",
            "蕒", "麦", "麥", "芒", "杧", "茅", "玫", "莓", "梅", "檬", "蒾",
            "棉", "蓂", "茉", "藦", "柰", "楠", "苨", "茑", "蔦", "菍", "苧",
            "柠", "檸", "藕", "蓬", "枇", "薸", "苹", "萍", "蘋", "桲", "葡",
            "蒲", "桤", "榿", "芪", "杞", "槭", "薺", "葜", "蕁", "芡", "茜",
            "蔷", "薔", "荞", "蕎", "茄", "芹", "芩", "檎", "苘", "筇", "萩",
            "楸", "蘘", "荛", "荏", "荵", "棯", "茸", "蓉", "榕", "薷", "箬",
            "桑", "莎", "杉", "穇", "芍", "苕", "蓍", "莳", "蒔", "柿", "薯",
            "蒴", "松", "菘", "苏", "蘇", "粟", "蒜", "荽", "笋", "筍", "梭",
            "蓑", "苔", "薹", "檀", "棠", "桃", "萄", "藤", "蓧", "葶", "蓪",
            "茼", "桐", "菟", "箨", "籜", "豌", "菀", "菵", "薇", "苇", "葦",
            "榅", "榲", "蕹", "莴", "萵", "梧", "菥", "樨", "秈", "籼", "莶",
            "薟", "藓", "蘚", "苋", "莧", "葙", "橡", "薤", "杏", "荇", "芎",
            "蓿", "萱", "薰", "荨", "栒", "桠", "椏", "芫", "蕘", "椰", "栘",
            "苡", "薏", "虉", "桜", "樱", "蘡", "櫻", "莜", "莸", "蕕", "莠",
            "柚", "萸", "榆", "萮", "芋", "橼", "櫞", "芸", "筠", "枣", "棗",
            "藻", "楂", "樟", "柘", "蔗", "榛", "芝", "栀", "梔", "蘵", "芷",
            "枳", "柊", "茱", "槠", "櫧", "竹", "苎", "梓"
    );
    private static final List<String> RESORT_PLANTS = shuffledPlantsForSalt(SALT);
    private static final Set<String> PLANT_SET = new HashSet<>(BASE_PLANTS);

    /**
     * 加密
     *
     * @param string 明文
     * @return 加密后的字符串（由植物名拼接而成）
     */
    public static String encode(String string) {
        if (string == null) string = "";

        // 1) 处理明文字节
        byte[] plainBytes = string.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] compressed = gzipCompress(plainBytes);
        boolean usedCompressed = compressed.length < plainBytes.length;
        byte[] payload;
        if (usedCompressed) {
            payload = new byte[1 + compressed.length];
            payload[0] = 1;
            System.arraycopy(compressed, 0, payload, 1, compressed.length);
        } else {
            payload = new byte[1 + plainBytes.length];
            payload[0] = 0;
            System.arraycopy(plainBytes, 0, payload, 1, plainBytes.length);
        }

        // 2) 派生 AES key
        SecretKeySpec key = deriveKeyFromSalt(SALT);

        // 3) 随机 IV，AES-GCM 加密
        byte[] iv = new byte[GCM_IV_BYTES];
        SECURE_RANDOM.nextBytes(iv);
        byte[] cipherBytes;
        try {
            Cipher cipher = Cipher.getInstance(AES_ALGO);
            GCMParameterSpec spec = new GCMParameterSpec(GCM_TAG_BITS, iv);
            cipher.init(Cipher.ENCRYPT_MODE, key, spec);
            cipherBytes = cipher.doFinal(payload);
        } catch (Exception e) {
            return "";
        }

        // 4) 组合 iv + 密文
        byte[] out = new byte[iv.length + cipherBytes.length];
        System.arraycopy(iv, 0, out, 0, iv.length);
        System.arraycopy(cipherBytes, 0, out, iv.length, cipherBytes.length);

        // 5) 使用打乱后的植物表进行 base-N 编码
        int base = RESORT_PLANTS.size();
        int[] digits = bytesToBaseNIndices(out, base); // 返回每位索引（0..base-1）
        StringBuilder sb = new StringBuilder(digits.length * TOKEN_CHAR_LENGTH);
        for (int d : digits) sb.append(RESORT_PLANTS.get(d));
        return sb.toString();
    }

    /**
     * 解密
     *
     * @param tokenString 密文
     * @return 解密后的明文（UTF-8）
     */
    public static String decode(String tokenString) {
        tokenString = replaceAroundLocator(tokenString);
        if (tokenString == null) tokenString = "";

        int base = RESORT_PLANTS.size();

        // 切分 tokenString 为 tokens
        if (tokenString.length() % TOKEN_CHAR_LENGTH != 0) {
            throw new IllegalArgumentException("Invalid tokenString length for provided token length");
        }
        int tokenCount = tokenString.length() / TOKEN_CHAR_LENGTH;
        int[] indices = new int[tokenCount];
        for (int i = 0; i < tokenCount; i++) {
            String token = tokenString.substring(i * TOKEN_CHAR_LENGTH, (i + 1) * TOKEN_CHAR_LENGTH);
            // binarySearch 要求 alphabet 已排序，故不用 binarySearch，改用 HashMap 加速查找
            int idx = Collections.binarySearch(RESORT_PLANTS, token);
        }

        // 构造 map token -> index（加速解码）
        Map<String, Integer> tokenToIndex = new HashMap<>(RESORT_PLANTS.size() * 2);
        for (int i = 0; i < RESORT_PLANTS.size(); i++) tokenToIndex.put(RESORT_PLANTS.get(i), i);

        for (int i = 0; i < tokenCount; i++) {
            String token = tokenString.substring(i * TOKEN_CHAR_LENGTH, (i + 1) * TOKEN_CHAR_LENGTH);
            Integer idx = tokenToIndex.get(token);
            if (idx == null) throw new IllegalArgumentException("Invalid token found during decode: " + token);
            indices[i] = idx;
        }

        // indices -> bytes
        byte[] combined = baseNIndicesToBytes(indices, base);

        // split iv + ciphertext
        if (combined.length < GCM_IV_BYTES + 1) throw new IllegalArgumentException("Decoded bytes too short");
        byte[] iv = Arrays.copyOfRange(combined, 0, GCM_IV_BYTES);
        byte[] ct = Arrays.copyOfRange(combined, GCM_IV_BYTES, combined.length);

        // derive key again
        SecretKeySpec key = deriveKeyFromSalt(SALT);

        byte[] payload;
        try {
            Cipher cipher = Cipher.getInstance(AES_ALGO);
            GCMParameterSpec spec = new GCMParameterSpec(GCM_TAG_BITS, iv);
            cipher.init(Cipher.DECRYPT_MODE, key, spec);
            payload = cipher.doFinal(ct);
        } catch (Exception e) {
            return "";
        }

        if (payload.length < 1) throw new IllegalArgumentException("Decrypted payload too short");
        boolean compressed = payload[0] == 1;
        byte[] data = Arrays.copyOfRange(payload, 1, payload.length);
        byte[] plainBytes = compressed ? gzipDecompress(data) : data;
        return new String(plainBytes, java.nio.charset.StandardCharsets.UTF_8);
    }

    public static boolean isPlantToken(String tokenString) {
        if (StringUtils.isNullOrEmptyEx(tokenString)) return false;
        String string = replaceAroundLocator(tokenString);
        return string.length() % TOKEN_CHAR_LENGTH == 0
                && PLANT_SET.containsAll(StringUtils.splitString(string, TOKEN_CHAR_LENGTH));

    }

    public static KeyValue<String, String> getAroundLocator(String tokenString) {
        if (StringUtils.isNullOrEmptyEx(tokenString)) return null;
        return LOCATOR.stream().filter(kv ->
                tokenString.startsWith(kv.getKey()) && tokenString.endsWith(kv.getValue())
        ).findFirst().orElse(null);
    }

    public static boolean isAroundLocator(String tokenString) {
        return getAroundLocator(tokenString) != null;
    }

    public static String replaceAroundLocator(String tokenString) {
        KeyValue<String, String> locator = getAroundLocator(tokenString);
        return locator == null
                ? tokenString
                : tokenString.replaceAll(String.format("^%s|%s$", locator.getKey(), locator.getValue()), "");
    }

    /**
     * 压缩字节数组
     */
    public static byte[] gzipCompress(byte[] data) {
        if (data == null || data.length == 0) {
            return null;
        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
             GZIPOutputStream gzip = new GZIPOutputStream(bos)) {
            gzip.write(data);
            gzip.finish();
            return bos.toByteArray();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 解压字节数组
     */
    public static byte[] gzipDecompress(byte[] compressedData) {
        if (compressedData == null || compressedData.length == 0) {
            return null;
        }
        try (ByteArrayInputStream bis = new ByteArrayInputStream(compressedData);
             GZIPInputStream gzip = new GZIPInputStream(bis);
             ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            byte[] buffer = new byte[1024];
            int len;
            while ((len = gzip.read(buffer)) > 0) {
                bos.write(buffer, 0, len);
            }
            return bos.toByteArray();
        } catch (Exception e) {
            return null;
        }
    }


    private static SecretKeySpec deriveKeyFromSalt(String salt) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            md.update(salt.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            byte[] digest = md.digest(); // 32 bytes
            return new SecretKeySpec(digest, "AES");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 对 PLANTS 做确定性打乱（基于 SALT -> sha-256 -> long seed）
     */
    private static List<String> shuffledPlantsForSalt(String salt) {
        // 复制原始表
        List<String> copy = new ArrayList<>(BASE_PLANTS);
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            md.update(salt.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            byte[] digest = md.digest();
            long seed = ByteBuffer.wrap(digest).getLong(); // 8 bytes -> long
            Random rnd = new Random(seed);
            // Fisher-Yates shuffle with rnd
            for (int i = copy.size() - 1; i > 0; i--) {
                int j = rnd.nextInt(i + 1);
                Collections.swap(copy, i, j);
            }
            return copy;
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 将 bytes (base 256) 转换为 base-N 索引数组（0..base-1）。
     * 保留前导 0x00 字节，通过在结果最前端填充 index 0 来表示前导零。
     */
    private static int[] bytesToBaseNIndices(byte[] input, int base) {
        if (base < 2) throw new IllegalArgumentException("base must be >= 2");
        // count leading zero bytes
        int leadingZeros = 0;
        while (leadingZeros < input.length && input[leadingZeros] == 0) leadingZeros++;

        // copy the non-zero suffix
        byte[] src = Arrays.copyOfRange(input, leadingZeros, input.length);

        List<Integer> digits = new ArrayList<>();
        if (src.length == 0) {
            // input was all zeros -> output single zero digit (plus leadingZeros handled below)
            digits.add(0);
        } else {
            // base conversion: repeatedly divide src by base, collecting remainders
            byte[] current = src;
            while (current.length > 0) {
                ByteArrayOutputStream next = new ByteArrayOutputStream();
                int carry = 0;
                for (byte b : current) {
                    int val = (carry << 8) | (b & 0xFF);
                    int q = val / base;
                    carry = val % base;
                    if (next.size() > 0 || q != 0) next.write(q);
                }
                digits.add(carry); // remainder
                current = next.toByteArray();
            }
            // digits currently least-significant first
            Collections.reverse(digits);
        }

        // prepend leadingZeros number of zeros (each corresponds to token index 0)
        int total = leadingZeros + digits.size();
        int[] out = new int[total];
        for (int i = 0; i < leadingZeros; i++) out[i] = 0;
        for (int i = 0; i < digits.size(); i++) out[leadingZeros + i] = digits.get(i);
        return out;
    }

    /**
     * 将 base-N 索引数组（0..base-1）转换回字节数组（base 256）
     */
    private static byte[] baseNIndicesToBytes(int[] indices, int base) {
        if (base < 2) throw new IllegalArgumentException("base must be >= 2");
        if (indices.length == 0) return new byte[0];

        // count leading zero indices
        int leadingZeroIndices = 0;
        while (leadingZeroIndices < indices.length && indices[leadingZeroIndices] == 0) leadingZeroIndices++;

        // work on the suffix
        int[] src = Arrays.copyOfRange(indices, leadingZeroIndices, indices.length);

        List<Byte> outBytes = new ArrayList<>();
        if (src.length == 0) {
            // all zeros -> produce empty suffix, but leadingZeroIndices will produce zero bytes below
        } else {
            // convert from base-N digits to base-256 bytes (schoolbook division)
            List<Integer> cur = new ArrayList<>();
            for (int v : src) {
                if (v < 0 || v >= base) throw new IllegalArgumentException("digit out of range");
                cur.add(v);
            }
            while (!cur.isEmpty()) {
                int carry = 0;
                List<Integer> next = new ArrayList<>();
                for (int d : cur) {
                    int acc = carry * base + d;
                    int q = acc / 256;
                    int r = acc % 256;
                    if (!next.isEmpty() || q != 0) next.add(q);
                    carry = r;
                }
                outBytes.add((byte) carry); // remainder is next byte (least-significant first)
                cur = next;
            }
            Collections.reverse(outBytes); // now big-endian bytes for suffix
        }

        // prepend leadingZeroIndices number of 0x00 bytes
        byte[] result = new byte[leadingZeroIndices + outBytes.size()];
        for (int i = 0; i < outBytes.size(); i++) result[leadingZeroIndices + i] = outBytes.get(i);
        // leading zeros default to 0
        return result;
    }

}
