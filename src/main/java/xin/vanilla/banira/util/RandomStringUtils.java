package xin.vanilla.banira.util;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

/**
 * 随机字符串生成工具类
 */
public final class RandomStringUtils {

    // 预定义字符集
    private static final String DIGITS = "0123456789";
    private static final String LOWERCASE_LETTERS = "abcdefghijklmnopqrstuvwxyz";
    private static final String UPPERCASE_LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    private static final String LETTERS = LOWERCASE_LETTERS + UPPERCASE_LETTERS;
    private static final String ALPHANUMERIC = DIGITS + LETTERS;
    private static final String SPECIAL_CHARACTERS = "!@#$%^&*()_+-=[]{}|;:,.<>?";
    private static final String ASCII_PRINTABLE =
            " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

    private RandomStringUtils() {
    }

    /**
     * 生成随机字符串
     *
     * @param length 字符串长度
     * @param source 字符源类型
     * @return 随机字符串
     */
    public static String generate(int length, CharSource source) {
        if (length <= 0) {
            throw new IllegalArgumentException("Length must be greater than 0");
        }

        return switch (source) {
            case DIGITS -> generateFromSource(length, DIGITS);
            case LOWERCASE_LETTERS -> generateFromSource(length, LOWERCASE_LETTERS);
            case UPPERCASE_LETTERS -> generateFromSource(length, UPPERCASE_LETTERS);
            case LETTERS -> generateFromSource(length, LETTERS);
            case ALPHANUMERIC -> generateFromSource(length, ALPHANUMERIC);
            case SPECIAL_CHARACTERS -> generateFromSource(length, SPECIAL_CHARACTERS);
            case ASCII_PRINTABLE -> generateFromSource(length, ASCII_PRINTABLE);
            case ANY_CHARACTER -> generateAnyCharacter(length);
        };
    }

    /**
     * 从自定义字符集中生成随机字符串
     *
     * @param length     字符串长度
     * @param characters 自定义字符集
     * @return 随机字符串
     */
    public static String generateFromCustom(int length, String characters) {
        if (length <= 0) {
            throw new IllegalArgumentException("Length must be greater than 0");
        }
        if (characters == null || characters.isEmpty()) {
            throw new IllegalArgumentException("Characters cannot be null or empty");
        }

        return generateFromSource(length, characters);
    }

    /**
     * 生成包含多种字符类型的随机字符串
     *
     * @param length    字符串长度
     * @param digits    是否包含数字
     * @param lowercase 是否包含小写字母
     * @param uppercase 是否包含大写字母
     * @param special   是否包含特殊字符
     */
    public static String generate(int length,
                                  boolean digits,
                                  boolean lowercase,
                                  boolean uppercase,
                                  boolean special) {

        if (length <= 0) {
            throw new IllegalArgumentException("Length must be greater than 0");
        }

        StringBuilder charPool = new StringBuilder();
        if (digits) charPool.append(DIGITS);
        if (lowercase) charPool.append(LOWERCASE_LETTERS);
        if (uppercase) charPool.append(UPPERCASE_LETTERS);
        if (special) charPool.append(SPECIAL_CHARACTERS);

        if (charPool.isEmpty()) {
            throw new IllegalArgumentException("No characters specified");
        }

        return generateFromSource(length, charPool.toString());
    }

    /**
     * 生成任意Unicode字符
     *
     * @param length 字符串长度
     */
    public static String generateAnyCharacter(int length) {
        if (length <= 0) {
            throw new IllegalArgumentException("Length must be greater than 0");
        }

        // 使用Stream生成随机Unicode字符
        return IntStream.generate(() -> ThreadLocalRandom.current().nextInt(Character.MIN_CODE_POINT, Character.MAX_CODE_POINT + 1))
                .filter(Character::isDefined)
                .limit(length)
                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
                .toString();
    }

    /**
     * 从指定字符源生成随机字符串
     */
    private static String generateFromSource(int length, String source) {
        Random random = ThreadLocalRandom.current();
        StringBuilder sb = new StringBuilder(length);

        for (int i = 0; i < length; i++) {
            int index = random.nextInt(source.length());
            sb.append(source.charAt(index));
        }

        return sb.toString();
    }

    /**
     * 字符源类型枚举
     */
    public enum CharSource {
        DIGITS,                  // 数字
        LOWERCASE_LETTERS,       // 小写字母
        UPPERCASE_LETTERS,       // 大写字母
        LETTERS,                 // 所有字母
        ALPHANUMERIC,            // 字母和数字
        SPECIAL_CHARACTERS,      // 特殊符号
        ASCII_PRINTABLE,         // 所有可打印ASCII字符
        ANY_CHARACTER            // 任意字符（包括Unicode字符）
    }


    public static void main(String[] args) {
        // 生成10位数字字符串
        System.out.println("数字: " + generate(10, CharSource.DIGITS));

        // 生成8位字母字符串
        System.out.println("字母: " + generate(8, CharSource.LETTERS));

        // 生成12位字母数字混合字符串
        System.out.println("字母数字: " + generate(12, CharSource.ALPHANUMERIC));

        // 生成包含特殊字符的字符串
        System.out.println("特殊字符: " + generate(15, CharSource.SPECIAL_CHARACTERS));

        // 生成任意Unicode字符
        System.out.println("任意字符: " + generateAnyCharacter(5));

        // 使用自定义字符集
        System.out.println("自定义: " + generateFromCustom(6, "甲乙丙丁戊己庚辛壬癸"));

        // 使用多种选项组合
        System.out.println("组合: " + generate(10, true, true, true, false));
    }
}
