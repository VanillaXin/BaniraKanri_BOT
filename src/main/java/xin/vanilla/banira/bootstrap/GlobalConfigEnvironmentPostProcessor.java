package xin.vanilla.banira.bootstrap;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

@Order(Ordered.HIGHEST_PRECEDENCE)
public class GlobalConfigEnvironmentPostProcessor implements EnvironmentPostProcessor {

    public GlobalConfigEnvironmentPostProcessor() {
    }

    @Override
    public void postProcessEnvironment(ConfigurableEnvironment environment, SpringApplication application) {
        File file = new File("./config/global-config.yml");
        if (!file.exists()) return;

        try {
            ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
            Map<String, Object> global = mapper.readValue(file, Map.class);

            String token = getValue(global, "token", String.class);
            String wsUrl = getValue(global, "wsUrl", String.class);

            Map<String, Object> overrideMap = new HashMap<>();
            overrideMap.put("shiro.ws.access-token", token);

            boolean client = wsUrl.startsWith("ws://");
            overrideMap.put("shiro.ws.client.enable", client);
            overrideMap.put("shiro.ws.server.enable", !client);

            if (client) {
                overrideMap.put("shiro.ws.client.url", wsUrl);
            } else {
                String[] split = wsUrl.split(":", 2);
                overrideMap.put("shiro.ws.server.url", split[0]);
                overrideMap.put("server.port", split[1]);
            }

            environment.getPropertySources().addFirst(
                    new MapPropertySource("globalConfigOverrides", overrideMap)
            );

        } catch (Exception e) {
            throw new IllegalStateException("Failed to load global config", e);
        }
    }

    private <T> T getValue(Map<String, Object> map, String key, Class<T> type) {
        Object val = map.get(key);
        if (val == null) return null;
        return type.cast(val);
    }
}
