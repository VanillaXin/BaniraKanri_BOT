package xin.vanilla.banira.plugin.mcmod;

import jakarta.annotation.Nonnull;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.McModUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.mcmod.McModContent;
import xin.vanilla.banira.util.mcmod.McModSearchResult;

import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * MCMod 百科检索，供插件指令与 AI 能力共用
 */
@Service
public class McModService {

    @Nonnull
    public String search(@Nonnull String type, @Nonnull String keyword) {
        if (StringUtils.isNullOrEmptyEx(keyword)) {
            return "关键词不能为空";
        }
        String normalized = type.trim().toLowerCase(Locale.ROOT);
        return switch (normalized) {
            case "mod", "模组" -> searchMod(keyword);
            case "modpack", "pack", "整合包" -> searchModpack(keyword);
            case "author", "作者" -> searchAuthor(keyword);
            case "user", "用户" -> searchUser(keyword);
            case "item", "data", "资料", "物品" -> searchData(keyword);
            case "tutorial", "教程" -> searchTutorial(keyword);
            case "random", "随便看看" -> randomMod();
            default -> "不支持的检索类型：" + type + "，可用 mod/modpack/author/user/item/tutorial/random";
        };
    }

    @Nonnull
    private String searchMod(@Nonnull String keyword) {
        if (isNumericId(keyword)) {
            McModContent direct = McModUtils.getModName(keyword);
            if (direct != null) {
                return formatContents(List.of(direct));
            }
        }
        List<McModContent> results = McModUtils.searchMod(keyword);
        return formatContents(results);
    }

    @Nonnull
    private String searchModpack(@Nonnull String keyword) {
        if (isNumericId(keyword)) {
            McModContent direct = McModUtils.getModpackName(keyword);
            if (direct != null) {
                return formatContents(List.of(direct));
            }
        }
        List<McModContent> results = McModUtils.searchModpack(keyword);
        return formatContents(results);
    }

    @Nonnull
    private String searchAuthor(@Nonnull String keyword) {
        if (isNumericId(keyword)) {
            McModContent direct = McModUtils.getAuthorName(keyword);
            if (direct != null) {
                return formatContents(List.of(direct));
            }
        }
        List<McModContent> results = McModUtils.searchAuthor(keyword);
        return formatContents(results);
    }

    @Nonnull
    private String searchUser(@Nonnull String keyword) {
        List<McModSearchResult> results = McModUtils.searchUserBySearchPage(keyword);
        return formatSearchResults(results);
    }

    @Nonnull
    private String searchData(@Nonnull String keyword) {
        List<McModSearchResult> results = McModUtils.searchDataBySearchPage(keyword);
        return formatSearchResults(results);
    }

    @Nonnull
    private String searchTutorial(@Nonnull String keyword) {
        List<McModSearchResult> results = McModUtils.searchTutorialBySearchPage(keyword);
        return formatSearchResults(results);
    }

    @Nonnull
    private String randomMod() {
        List<McModContent> randomMods = McModUtils.getRandomMods();
        return formatContents(randomMods);
    }

    @Nonnull
    private static String formatContents(@Nonnull List<McModContent> results) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            return "未找到相关内容";
        }
        return results.stream()
                .limit(8)
                .map(content -> content.getFormattedName() + " | " + content.getDetailUrl())
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    private static String formatSearchResults(@Nonnull List<McModSearchResult> results) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            return "未找到相关内容";
        }
        return results.stream()
                .limit(8)
                .map(result -> result.getTitle() + " | " + result.getLink())
                .collect(Collectors.joining("\n"));
    }

    private static boolean isNumericId(@Nonnull String keyword) {
        return keyword.matches("\\d+");
    }

}
