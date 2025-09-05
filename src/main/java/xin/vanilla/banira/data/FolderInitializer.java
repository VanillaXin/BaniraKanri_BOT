package xin.vanilla.banira.data;

import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@Component
public class FolderInitializer implements SmartInitializingSingleton {

    @Value("${spring.datasource.url}")
    private String url;

    private static final String[] paths = new String[]{
            "data",
            "config",
            "cache/image",
            "cache/video",
            "cache/file",
    };

    @Override
    public void afterSingletonsInstantiated() {
        if (url.startsWith("jdbc:sqlite:")) {
            String path = url.substring("jdbc:sqlite:".length());
            Path dbPath = Paths.get(path).toAbsolutePath();
            try {
                Files.createDirectories(dbPath.getParent());
            } catch (IOException e) {
                throw new IllegalStateException("Failed to create directory for database", e);
            }
        }

        for (String path : paths) {
            Path dataPath = Paths.get(path).toAbsolutePath();
            try {
                Files.createDirectories(dataPath);
            } catch (IOException e) {
                throw new IllegalStateException("Failed to create directory for " + path, e);
            }
        }
    }
}
