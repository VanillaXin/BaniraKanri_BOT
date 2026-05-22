package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 群管指令处理器注册表
 */
@Slf4j
@Component
public class KanriHandlerRegistry {

    /**
     * AI 可调用的群管动作白名单（不含踢人、权限/角色/管理员名单、撤回、@全体等）
     */
    private static final Set<Class<?>> AI_ALLOWED_HANDLERS = Set.of(
            MuteCommand.class,
            LoudCommand.class,
            CardCommand.class,
            TagCommand.class,
            EssenceCommand.class,
            GroupNameCommand.class
    );

    @Autowired(required = false)
    private List<KanriHandler> handlers = new ArrayList<>();

    private final Map<String, KanriHandler> handlerByAction = new HashMap<>();
    private final Map<String, KanriHandler> aiHandlerByAction = new HashMap<>();

    @PostConstruct
    public void init() {
        handlerByAction.clear();
        aiHandlerByAction.clear();
        for (KanriHandler handler : handlers) {
            for (String action : handler.getAction()) {
                String actionKey = normalizeAction(action);
                if (StringUtils.isNullOrEmptyEx(actionKey)) {
                    continue;
                }
                register(handlerByAction, actionKey, handler);
                if (isAiAllowed(handler)) {
                    register(aiHandlerByAction, actionKey, handler);
                }
            }
        }
    }

    @Nonnull
    public Collection<KanriHandler> allHandlers() {
        return handlers;
    }

    @Nullable
    public KanriHandler resolve(@Nullable String action) {
        return handlerByAction.get(normalizeAction(action));
    }

    @Nullable
    public KanriHandler resolveForAi(@Nullable String action) {
        return aiHandlerByAction.get(normalizeAction(action));
    }

    @Nonnull
    public Map<String, KanriHandler> aiHandlers() {
        return Collections.unmodifiableMap(aiHandlerByAction);
    }

    public boolean isAiAllowed(@Nonnull KanriHandler handler) {
        return AI_ALLOWED_HANDLERS.contains(handler.getClass());
    }

    @Nonnull
    private static String normalizeAction(@Nullable String action) {
        return action == null ? "" : action.trim().toLowerCase(Locale.ROOT);
    }

    private static void register(@Nonnull Map<String, KanriHandler> map, @Nonnull String actionKey, @Nonnull KanriHandler handler) {
        KanriHandler existed = map.putIfAbsent(actionKey, handler);
        if (existed != null && existed != handler) {
            LOGGER.warn("Duplicate kanri action '{}' between handlers '{}' and '{}', keep '{}'",
                    actionKey,
                    existed.getClass().getSimpleName(),
                    handler.getClass().getSimpleName(),
                    existed.getClass().getSimpleName());
        }
    }

}
