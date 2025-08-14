package xin.vanilla.banira.data;

import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.plugin.*;
import org.apache.ibatis.reflection.MetaObject;
import org.apache.ibatis.reflection.SystemMetaObject;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;

import java.sql.Connection;
import java.util.Properties;

@Intercepts({
        @Signature(type = StatementHandler.class, method = "prepare", args = {Connection.class, Integer.class})
})
public class SQLitePageInterceptor implements Interceptor {

    @Override
    public Object intercept(Invocation invocation) throws Throwable {
        StatementHandler statementHandler = (StatementHandler) invocation.getTarget();
        MetaObject metaObject = SystemMetaObject.forObject(statementHandler);

        String sql = (String) metaObject.getValue("delegate.boundSql.sql");

        Object paramObject = metaObject.getValue("delegate.boundSql.parameterObject");
        if (paramObject instanceof java.util.Map<?, ?> paramMap) {
            Object pageObj = paramMap.get(BaniraQueryParam.PAGE);
            Object sizeObj = paramMap.get(BaniraQueryParam.SIZE);

            Object limitObj = paramMap.get(BaniraQueryParam.LIMIT);
            Object offsetObj = paramMap.get(BaniraQueryParam.OFFSET);

            if (((pageObj instanceof Number page) && page.longValue() >= 1)
                    && ((sizeObj instanceof Number size) && size.longValue() > 0)
            ) {
                long offset = (page.longValue() - 1) * size.longValue();
                sql = sql + " LIMIT " + size.longValue() + " OFFSET " + offset;
                metaObject.setValue("delegate.boundSql.sql", sql);
            } else if (((limitObj instanceof Number limit) && limit.longValue() >= 1)
                    && ((offsetObj instanceof Number offset) && offset.longValue() >= 0)
            ) {
                sql = sql + " LIMIT " + limit.longValue() + " OFFSET " + offset.longValue();
                metaObject.setValue("delegate.boundSql.sql", sql);
            }
        }

        return invocation.proceed();
    }

    @Override
    public Object plugin(Object target) {
        return Plugin.wrap(target, this);
    }

    @Override
    public void setProperties(Properties properties) {
    }
}
