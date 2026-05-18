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
import java.net.URI;
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
            String env = getValue(global, "env", String.class);
            if (token == null || token.isBlank()) {
                throw new IllegalArgumentException("global-config.yml token is required");
            }
            if (wsUrl == null || wsUrl.isBlank()) {
                throw new IllegalArgumentException("global-config.yml wsUrl is required");
            }
            if ("dev".equalsIgnoreCase(env)) env = "dev";
            else env = "prod";

            Map<String, Object> overrideMap = new HashMap<>();
            overrideMap.put("spring.profiles.active", env);

            overrideMap.put("shiro.ws.access-token", token);

            boolean client = wsUrl.startsWith("ws://");
            overrideMap.put("shiro.ws.client.enable", client);
            overrideMap.put("shiro.ws.server.enable", !client);

            if (client) {
                overrideMap.put("shiro.ws.client.url", wsUrl);
            } else {
                Map<String, String> serverEndpoint = parseServerEndpoint(wsUrl);
                overrideMap.put("shiro.ws.server.url", serverEndpoint.get("host"));
                overrideMap.put("server.port", serverEndpoint.get("port"));
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

    private Map<String, String> parseServerEndpoint(String wsUrl) {
        try {
            String host;
            int port;
            if (wsUrl.contains("://")) {
                URI uri = URI.create(wsUrl);
                host = uri.getHost();
                port = uri.getPort();
            } else {
                int splitIndex = wsUrl.lastIndexOf(':');
                if (splitIndex <= 0 || splitIndex == wsUrl.length() - 1) {
                    throw new IllegalArgumentException("Invalid server wsUrl format, expected host:port");
                }
                host = wsUrl.substring(0, splitIndex);
                port = Integer.parseInt(wsUrl.substring(splitIndex + 1));
            }
            if (host == null || host.isBlank() || port <= 0 || port > 65535) {
                throw new IllegalArgumentException("Invalid server wsUrl host or port");
            }
            Map<String, String> endpoint = new HashMap<>();
            endpoint.put("host", host);
            endpoint.put("port", String.valueOf(port));
            return endpoint;
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid wsUrl in global-config.yml: " + wsUrl, e);
        }
    }
}
