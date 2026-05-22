package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 从机器人口头禁言提议中解析目标昵称与时长。
 */
public final class KanriProposalParser {

    private static final Pattern MUTE_MINUTES = Pattern.compile("禁言\\s*(\\d+)\\s*分钟");

    private KanriProposalParser() {
    }

    public static boolean containsMuteProposal(@Nullable String speech) {
        if (StringUtils.isNullOrEmptyEx(speech)) {
            return false;
        }
        return speech.contains("禁言") && !extractMuteTargetKeywords(speech).isEmpty();
    }

    @Nonnull
    public static List<String> extractMuteTargetKeywords(@Nullable String speech) {
        if (StringUtils.isNullOrEmptyEx(speech) || !speech.contains("禁言")) {
            return List.of();
        }
        int muteIndex = speech.indexOf("禁言");
        String beforeMute = speech.substring(0, muteIndex);
        int colon = Math.max(beforeMute.lastIndexOf('：'), beforeMute.lastIndexOf(':'));
        String targetPart = colon >= 0 ? beforeMute.substring(colon + 1) : beforeMute;
        targetPart = targetPart.replaceAll(".*(那几个|他们|这几个人|闹的那几个|如下)", "").trim();
        if (StringUtils.isNullOrEmptyEx(targetPart)) {
            return List.of();
        }
        List<String> keywords = new ArrayList<>();
        for (String part : targetPart.split("[、，,\\s]+")) {
            String trimmed = part.trim();
            if (trimmed.length() >= 2 && !trimmed.matches("\\d+")) {
                keywords.add(trimmed);
            }
        }
        return keywords;
    }

    public static int extractMuteMinutes(@Nullable String speech, int defaultMinutes) {
        if (StringUtils.isNullOrEmptyEx(speech)) {
            return defaultMinutes;
        }
        Matcher matcher = MUTE_MINUTES.matcher(speech);
        if (matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        }
        return defaultMinutes;
    }
}
