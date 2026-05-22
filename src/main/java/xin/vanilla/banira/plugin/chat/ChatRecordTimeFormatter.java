package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public final class ChatRecordTimeFormatter {

    private static final ZoneId ZONE = ZoneId.systemDefault();
    private static final DateTimeFormatter ABSOLUTE = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private ChatRecordTimeFormatter() {
    }

    @Nonnull
    public static String format(@Nullable Long epochSeconds) {
        if (epochSeconds == null || epochSeconds <= 0) {
            return "unknown";
        }
        var dateTime = Instant.ofEpochSecond(epochSeconds).atZone(ZONE).toLocalDateTime();
        LocalDate date = dateTime.toLocalDate();
        LocalDate today = LocalDate.now(ZONE);
        String relative;
        if (date.equals(today)) {
            relative = "today";
        } else if (date.equals(today.minusDays(1))) {
            relative = "yesterday";
        } else {
            relative = "date";
        }
        return ABSOLUTE.format(dateTime) + " " + relative;
    }
}
