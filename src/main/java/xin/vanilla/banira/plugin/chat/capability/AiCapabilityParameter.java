package xin.vanilla.banira.plugin.chat.capability;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class AiCapabilityParameter {
    private String name = "";
    private String description = "";
    private boolean required;

    public static AiCapabilityParameter required(String name, String description) {
        return new AiCapabilityParameter()
                .name(name)
                .description(description)
                .required(true);
    }

    public static AiCapabilityParameter optional(String name, String description) {
        return new AiCapabilityParameter()
                .name(name)
                .description(description)
                .required(false);
    }
}
