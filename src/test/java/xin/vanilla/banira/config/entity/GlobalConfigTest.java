package xin.vanilla.banira.config.entity;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class GlobalConfigTest {

    private final ObjectMapper mapper = new ObjectMapper(new YAMLFactory());

    @Test
    void shouldReadLegacySingleBotNickAsList() throws Exception {
        GlobalConfig config = mapper.readValue("""
                botNick: "白茶酱"
                """, GlobalConfig.class);

        Assertions.assertEquals(List.of("白茶酱"), config.botNick());
    }

    @Test
    void shouldReadMultipleBotNicks() throws Exception {
        GlobalConfig config = mapper.readValue("""
                botNick:
                - "白茶酱"
                - "香草白茶"
                """, GlobalConfig.class);

        Assertions.assertEquals(List.of("白茶酱", "香草白茶"), config.botNick());
    }
}
