package xin.vanilla.banira.mapper.common;

import lombok.Getter;
import lombok.Setter;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

@Getter
@Setter
@SuppressWarnings({"unused"})
public class BaniraQueryParam extends HashMap<String, Object> {
    public static final String QUERY_ATTR_KEY_WORD = "_KEY_WORD_";

    public static final String ALL_FIELDS = "_ALL_FIELDS_";
    public static final String ORDER = "_ORDER_";

    private long startIndex = 0;
    private long pageSize = -1;

    /**
     * 指定返回的记录数量
     */
    private long limit;
    /**
     * 指定跳过的记录数量
     */
    private long offset;

    public BaniraQueryParam() {
        this(false);
    }

    public BaniraQueryParam(boolean all) {
        this.put(ALL_FIELDS, all);
    }

    public BaniraQueryParam(long startIndex, long pageSize) {
        this(false);
        this.startIndex = startIndex;
        this.pageSize = pageSize;
    }

    public BaniraQueryParam(boolean all, long startIndex, long pageSize) {
        this(all);
        this.startIndex = startIndex;
        this.pageSize = pageSize;
    }

    public BaniraQueryParam addParam(String key, Object value) {
        if (value != null) {
            switch (value) {
                case Number number when number.doubleValue() == 0 -> {
                }
                case String string when string.isEmpty() -> {
                }
                case Collection<?> collection when collection.isEmpty() -> {
                }
                default -> this.put(key, value);
            }
        }
        return this;
    }

    public BaniraQueryParam addParamAllowEmpty(String key, Object value) {
        if (value == null) this.put(key + "_null", null);
        else this.put(key, value);
        return this;
    }

    public BaniraQueryParam addParam(String name, Object... values) {
        this.put(name + "_array", values);
        return this;
    }

    public BaniraQueryParam addParamByArray(String name, Object[] values) {
        this.put(name + "_array", values);
        return this;
    }

    public BaniraQueryParam addParamByList(String name, Collection<?> values) {
        this.put(name + "_list", values);
        return this;
    }

    public BaniraQueryParam addParamByOr(String name, Object... values) {
        this.put(name + "_array_or", values);
        return this;
    }

    public BaniraQueryParam addParamByArrayOr(String name, Object[] values) {
        this.put(name + "_array_or", values);
        return this;
    }

    public BaniraQueryParam addParamByListOr(String name, Collection<?> values) {
        this.put(name + "_list_or", values);
        return this;
    }

    public BaniraQueryParam addParam(String key, String value) {
        if (StringUtils.isNotNullOrEmpty(value)) {
            this.put(key, value);
        }
        return this;
    }

    public BaniraQueryParam addParamAllowEmpty(String key, String value) {
        if (value == null) this.put(key + "_null", null);
        else this.put(key, value);
        return this;
    }

    /**
     * val < max
     */
    public BaniraQueryParam addParamByLt(String name, Object max) {
        this.put(name + "_lt", max);
        return this;
    }

    /**
     * val <= max
     */
    public BaniraQueryParam addParamByLtAndEqual(String name, Object max) {
        this.put(name + "_lt_eq", max);
        return this;
    }

    /**
     * val > min
     */
    public BaniraQueryParam addParamByGt(String name, Object min) {
        this.put(name + "_gt", min);
        return this;
    }

    /**
     * val >= min
     */
    public BaniraQueryParam addParamByGtAndEqual(String name, Object min) {
        this.put(name + "_gt_eq", min);
        return this;
    }

    /**
     * min <= val <= max
     */
    public BaniraQueryParam addParamByRangeOpen(String name, Object min, Object max) {
        this.addParamByGtAndEqual(name, min);
        this.addParamByLtAndEqual(name, max);
        return this;
    }

    /**
     * min < val < max
     */
    public BaniraQueryParam addParamByRangeClose(String name, Object min, Object max) {
        this.addParamByGt(name, min);
        this.addParamByLt(name, max);
        return this;
    }

    @SuppressWarnings("unchecked")
    public BaniraQueryParam addOrderBy(String name, boolean asc) {
        String order = String.format("%s_%s", name, asc ? "asc" : "desc");
        ((List<String>) this.computeIfAbsent(ORDER, k -> new ArrayList<String>())).add(order);
        return this;
    }
}
