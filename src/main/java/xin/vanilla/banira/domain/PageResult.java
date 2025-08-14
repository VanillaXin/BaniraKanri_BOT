package xin.vanilla.banira.domain;

import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Data
@Accessors(chain = true)
public class PageResult<T> {
    private long total;
    private long page;
    private long size;
    private long totalPages;
    private List<T> records;

    public PageResult(long total, long page, long size, List<T> records) {
        this.total = total;
        this.page = page;
        this.size = size;
        this.records = records;
        this.totalPages = (size > 0) ? ((total + size - 1) / size) : 0;
    }

    public PageResult(Collection<T> result) {
        this.records = CollectionUtils.isNotNullOrEmpty(result) ? (List<T>) result : new ArrayList<>();
        this.total = this.records.size();
        this.page = 1;
        this.size = this.records.size();
        this.totalPages = 1;
    }

    public PageResult<T> setTotal(long total) {
        this.total = total;
        this.totalPages = (size > 0) ? ((total + size - 1) / size) : 0;
        return this;
    }

    public boolean isEmpty() {
        return CollectionUtils.isNullOrEmpty(this.records);
    }

}
