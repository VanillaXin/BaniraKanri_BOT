package xin.vanilla.banira.util;


import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * 存储单位换算
 **/
public class StorageUnitUtils {

    public static final BigDecimal UNIT = BigDecimal.valueOf(1024);
    public static final BigDecimal BIT = BigDecimal.valueOf(1);
    public static final BigDecimal BYTE = BigDecimal.valueOf(8);
    public static final BigDecimal KB = BYTE.multiply(UNIT);
    public static final BigDecimal MB = KB.multiply(UNIT);
    public static final BigDecimal GB = MB.multiply(UNIT);
    public static final BigDecimal TB = GB.multiply(UNIT);
    public static final BigDecimal PB = TB.multiply(UNIT);
    public static final BigDecimal EB = PB.multiply(UNIT);
    public static final BigDecimal ZB = EB.multiply(UNIT);
    public static final BigDecimal YB = ZB.multiply(UNIT);
    public static final BigDecimal BB = YB.multiply(UNIT);
    public static final BigDecimal NB = BB.multiply(UNIT);
    public static final BigDecimal DB = NB.multiply(UNIT);
    public static final Integer SCALE = 2;

    public static final BigDecimal[] UNITS = {
            DB, NB, BB, YB, ZB, EB, PB, TB, GB, MB, KB, BYTE, BIT
    };
    public static final String[] UNIT_NAMES = {
            "DB", "NB", "BB", "YB", "ZB", "EB", "PB", "TB", "GB", "MB", "KB", "BYTE", "BIT"
    };

    /**
     * 存储单位换算
     *
     * @param length      需要转换的存储大小
     * @param currentUnit 当前存储单位
     * @param targetUnit  转换目标存储单位
     * @param scale       小数点
     **/
    public static BigDecimal convert(BigDecimal length, BigDecimal currentUnit, BigDecimal targetUnit, Integer scale) {
        if (scale == null) {
            scale = SCALE;
        }
        if (currentUnit.compareTo(targetUnit) < 0) {
            BigDecimal b1 = targetUnit.divide(currentUnit, RoundingMode.HALF_UP);
            return length.divide(b1, scale, RoundingMode.HALF_UP);
        } else if (currentUnit.compareTo(targetUnit) > 0) {
            BigDecimal b1 = currentUnit.divide(targetUnit, RoundingMode.HALF_UP);
            return length.multiply(b1).setScale(scale, RoundingMode.HALF_UP);
        } else {
            return length;
        }
    }

    /**
     * 存储单位换算，自动判断目标单位
     */
    public static String convert(BigDecimal length, BigDecimal currentUnit, Integer scale) {
        if (scale == null) {
            scale = SCALE;
        }
        if (BigDecimal.ZERO.compareTo(length) == 0) {
            return "0 BIT";
        }

        BigDecimal totalBits = length.multiply(currentUnit);

        for (int i = 0; i < UNITS.length; i++) {
            BigDecimal unitVal = UNITS[i];
            // 计算当前单位下的数值
            BigDecimal result = totalBits.divide(unitVal, scale + 2, RoundingMode.HALF_UP);

            // 判断条件：结果 >= 1 或 已是最后一个单位
            if (result.compareTo(BigDecimal.ONE) >= 0 || i == UNITS.length - 1) {
                // 处理临界值
                if (result.compareTo(UNIT) >= 0 && i > 0) {
                    // 进位到更大的单位
                    result = result.divide(UNIT, scale + 2, RoundingMode.HALF_UP);
                    return result.setScale(scale, RoundingMode.HALF_UP) + UNIT_NAMES[i - 1];
                }
                // 正常返回
                return result.setScale(scale, RoundingMode.HALF_UP) + UNIT_NAMES[i];
            }
        }
        return totalBits.setScale(scale, RoundingMode.HALF_UP) + "BIT";
    }

}
