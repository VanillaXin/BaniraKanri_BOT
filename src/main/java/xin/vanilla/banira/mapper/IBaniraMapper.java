package xin.vanilla.banira.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.apache.ibatis.session.ResultHandler;
import xin.vanilla.banira.mapper.param.BaniraQueryParam;

import java.util.List;

public interface IBaniraMapper<T, Q extends BaniraQueryParam> extends BaseMapper<T> {

    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    default int deleteByParam(Q queryParam) {
        return this.delete(Wrappers.<T>query().allEq(queryParam));
    }

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    default List<T> selectByParam(Q queryParam) {
        return this.selectList(Wrappers.<T>query().allEq(queryParam));
    }

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam    表字段 map 对象
     * @param resultHandler resultHandler 结果处理器 {@link ResultHandler}
     */
    default void selectByParam(Q queryParam, ResultHandler<T> resultHandler) {
        this.selectList(Wrappers.<T>query().allEq(queryParam), resultHandler);
    }

}
