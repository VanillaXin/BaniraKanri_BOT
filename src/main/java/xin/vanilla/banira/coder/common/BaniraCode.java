package xin.vanilla.banira.coder.common;

import com.google.gson.JsonObject;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class BaniraCode {

    private String type;

    private String raw;

    private JsonObject data;

}
