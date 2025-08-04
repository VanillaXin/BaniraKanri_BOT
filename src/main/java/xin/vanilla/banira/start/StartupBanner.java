package xin.vanilla.banira.start;

import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.net.UnknownHostException;

@Component
public class StartupBanner implements ApplicationListener<ApplicationReadyEvent> {

    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        Environment env = event.getApplicationContext().getEnvironment();

        int port = -1;
        if (event.getApplicationContext() instanceof WebServerApplicationContext webCtx) {
            port = webCtx.getWebServer().getPort();
        } else {
            String portStr = env.getProperty("local.server.port");
            if (portStr != null) {
                try {
                    port = Integer.parseInt(portStr);
                } catch (NumberFormatException ignored) {
                }
            }
        }

        String contextPath = env.getProperty("server.servlet.context-path", "");
        if (contextPath.isBlank()) contextPath = "/";

        String hostAddress = "unknown";
        try {
            InetAddress local = InetAddress.getLocalHost();
            hostAddress = local.getHostAddress();
        } catch (UnknownHostException ignored) {
        }

        StringBuilder sb = new StringBuilder(System.lineSeparator());
        sb.append("Application started successfully!").append(System.lineSeparator());
        sb.append("Accessible URLs:").append(System.lineSeparator());
        if (port != -1) {
            sb.append("    Local:      http://127.0.0.1:").append(port).append(contextPath).append(System.lineSeparator());
            sb.append("    External:   http://").append(hostAddress).append(":").append(port).append(contextPath).append(System.lineSeparator());
        } else {
            sb.append("    (Could not resolve port)").append(System.lineSeparator());
        }
        sb.append("    Context Path: ").append(contextPath).append(System.lineSeparator());

        System.out.println(sb);
    }
}
