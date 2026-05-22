package xin.vanilla.banira.plugin;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.List;

class McModPluginKeywordVariantTest {

    @SuppressWarnings("unchecked")
    @Test
    void shouldCreateCommonMcModKeywordVariants() throws Exception {
        Method method = McModPlugin.class.getDeclaredMethod("mcModKeywordVariants", String.class);
        method.setAccessible(true);

        List<String> variants = (List<String>) method.invoke(null, "竹叶清");

        Assertions.assertTrue(variants.contains("竹叶清"));
        Assertions.assertTrue(variants.contains("竹叶青"));
    }
}
