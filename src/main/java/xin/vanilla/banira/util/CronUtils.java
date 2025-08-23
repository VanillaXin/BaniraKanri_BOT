package xin.vanilla.banira.util;

import org.quartz.CronExpression;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * Cron表达式检查工具（基于 Quartz CronExpression）
 */
public final class CronUtils {

    private CronUtils() {
    }

    /**
     * 检查 cron 表达式未来若干次触发中，是否存在“过多”的相邻两次触发间隔小于 min 秒的情况。
     * 默认判定规则：如果相邻间隔中有 >= 75% （向上取整）小于 min 秒，则认为存在“过短”情况。
     *
     * @param cron  cron 表达式（Quartz 语法）
     * @param min   最小允许间隔（秒）
     * @param count 向前计算的触发次数（>=2）
     */
    public static boolean hasTooShortInterval(String cron, long min, int count) {
        return hasTooShortInterval(cron, min, count, 0.75, new Date());
    }

    /**
     * 检查 cron 表达式未来若干次触发中，是否存在“过多”的相邻两次触发间隔小于 min 秒的情况。
     *
     * @param cron  cron 表达式
     * @param min   最小允许间隔（秒）
     * @param count 向前计算的触发次数（>=2）
     * @param ratio 判定比例（例如 0.75 表示 >=75% 的间隔小于 min 则判定为过短）
     */
    public static boolean hasTooShortInterval(String cron, long min, int count, double ratio) {
        return hasTooShortInterval(cron, min, count, ratio, new Date());
    }

    /**
     * 检查 cron 表达式未来若干次触发中，是否存在“过多”的相邻两次触发间隔小于 min 秒的情况。
     *
     * @param cron  cron 表达式
     * @param min   最小允许间隔（秒）
     * @param count 向前计算的触发次数（>=2）
     * @param ratio 判定比例 (0 < ratio <= 1)
     * @param start 起始时间（包含该时间之后的触发）
     */
    public static boolean hasTooShortInterval(String cron, long min, int count, double ratio, Date start) {
        if (cron == null || cron.isBlank()) throw new IllegalArgumentException("cron must not be null/blank");
        if (min < 0) throw new IllegalArgumentException("min must be >= 0");
        if (count < 2) throw new IllegalArgumentException("count must be >= 2");
        if (!(ratio > 0 && ratio <= 1)) throw new IllegalArgumentException("ratio must be in (0, 1]");

        List<Date> times = getNextFireTimes(cron, count, start);
        if (times.size() < 2) return false;

        int intervals = times.size() - 1;
        int shortCount = 0;
        for (int i = 1; i < times.size(); i++) {
            long diffMillis = times.get(i).getTime() - times.get(i - 1).getTime();
            long seconds = diffMillis / 1000;
            if (seconds < min) shortCount++;
        }

        int threshold = (int) Math.ceil(intervals * ratio);
        return shortCount >= threshold;
    }

    /**
     * 检查 cron 表达式是否合法（Quartz）
     */
    public static boolean isValidCron(String cron) {
        if (cron == null || cron.isBlank()) return false;
        return CronExpression.isValidExpression(cron);
    }

    /**
     * 获取从 start 时间之后的 count 个触发时间
     *
     * @param cron  cron 表达式
     * @param count 期望返回的触发次数
     * @param start 起始时间
     */
    public static List<Date> getNextFireTimes(String cron, int count, Date start) {
        if (cron == null || cron.isBlank()) return Collections.emptyList();
        if (count < 1) throw new IllegalArgumentException("count must be >= 1");

        if (!isValidCron(cron)) return Collections.emptyList();

        try {
            CronExpression expression = new CronExpression(cron);
            Date cursor = (start == null) ? new Date() : new Date(Math.max(start.getTime(), System.currentTimeMillis()));
            List<Date> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                Date next = expression.getNextValidTimeAfter(cursor);
                if (next == null) break;
                result.add(next);
                // 向后推进1ms
                cursor = new Date(next.getTime() + 1);
            }
            return result;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    /**
     * 从当前时间开始取 count 次触发时间
     */
    public static List<Date> getNextFireTimes(String cron, int count) {
        return getNextFireTimes(cron, count, new Date());
    }

}
