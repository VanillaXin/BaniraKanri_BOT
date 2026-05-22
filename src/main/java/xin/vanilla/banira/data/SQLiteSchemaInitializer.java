package xin.vanilla.banira.data;

import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.event.DatabaseInitializedEvent;

import javax.sql.DataSource;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * sqlite schema initializer
 */
@Component
public class SQLiteSchemaInitializer implements ApplicationRunner {

    @jakarta.annotation.Resource
    private DataSource dataSource;

    @jakarta.annotation.Resource
    private ApplicationContext applicationContext;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        try (Connection conn = dataSource.getConnection()) {
            Statement stmt = conn.createStatement();
            executeSqlResource(stmt, "sql/1.init.sql");
            migrate(stmt);
        }
        applicationContext.publishEvent(new DatabaseInitializedEvent(this));
    }

    private void executeSqlResource(Statement stmt, String resourcePath) throws Exception {
        Resource resource = new ClassPathResource(resourcePath);
        String sql = new String(resource.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        for (String part : sql.split(";")) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty()) {
                try {
                    stmt.execute(trimmed);
                } catch (SQLException e) {
                    if (isDeferredMigrationSql(trimmed, e)) {
                        continue;
                    }
                    throw e;
                }
            }
        }
    }

    private boolean isDeferredMigrationSql(String sql, SQLException e) {
        String normalized = sql.toLowerCase();
        String message = e.getMessage() != null ? e.getMessage().toLowerCase() : "";
        return normalized.contains("recalled_index")
                && normalized.contains("message_record")
                && message.contains("no such column: recalled");
    }

    private void migrate(Statement stmt) {
        try {
            stmt.execute("ALTER TABLE minecraft_record ADD COLUMN rcon_operators TEXT NOT NULL DEFAULT ''");
        } catch (SQLException ignored) {
            // 列已存在
        }
        try {
            stmt.execute("ALTER TABLE message_record ADD COLUMN recalled BOOLEAN NOT NULL DEFAULT FALSE");
        } catch (SQLException ignored) {
            // column already exists
        }
        try {
            stmt.execute("CREATE INDEX IF NOT EXISTS recalled_index ON message_record (`recalled`)");
        } catch (SQLException ignored) {
            // best effort migration
        }
    }
}
