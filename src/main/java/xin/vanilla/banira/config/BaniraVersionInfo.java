package xin.vanilla.banira.config;

import lombok.Getter;
import lombok.experimental.Accessors;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.util.JavaVersionUtils;

import java.util.jar.Attributes;
import java.util.jar.Manifest;

@Getter
@Component
@Accessors(fluent = true)
public class BaniraVersionInfo {

    @Value("${git.commit.id.abbrev:}")
    private String gitCommitId;

    @Value("${git.build.version:}")
    private String gitBuildVersion;

    @Value("${git.commit.time:}")
    private String gitCommitTime;

    @Value("${git.commit.count:}")
    private String gitCommitCount;

    @Value("${git.last.commit.short:}")
    private String gitCommitShortId;


    public String getVersion() {
        try {
            ClassPathResource resource = new ClassPathResource("META-INF/MANIFEST.MF");
            Manifest manifest = new Manifest(resource.getInputStream());
            Attributes attributes = manifest.getMainAttributes();
            return attributes.getValue("Implementation-Version");
        } catch (Exception e) {
            return "UNKNOWN";
        }
    }

    public String getJavaVersion() {
        try {
            ClassPathResource resource = new ClassPathResource("META-INF/MANIFEST.MF");
            Manifest manifest = new Manifest(resource.getInputStream());
            Attributes attributes = manifest.getMainAttributes();
            return attributes.getValue("Built-Java");
        } catch (Exception e) {
            return JavaVersionUtils.getClassVersion(BaniraVersionInfo.class);
        }
    }
}
