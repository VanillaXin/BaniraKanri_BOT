package xin.vanilla.banira.coder;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.RegUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 转发至群组
 */
@Component
public class ToGroupCode implements BaniraCoder {

    private static final Pattern PATTERN = new RegUtils()
            .append(MSG_CODE_START)
            .groupNonIg("[tT][gG]|2[gG]|[tT]o[gG]roup|2[gG]roup")
            .append(ARG_SEPARATOR)
            .groupIgByName("group", "\\d{5,10}")
            .append(MSG_CODE_END)
            .compile();

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    @Override
    public boolean match(String msg) {
        return PATTERN.matcher(msg).find();
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context) {
        String msg = context.getMsg();
        Matcher matcher = PATTERN.matcher(msg);
        while (matcher.find()) {
            String group = matcher.group("group");
            long groupId = StringUtils.toLong(group);
            if (BaniraUtils.isGroupIdValid(groupId)) {
                context.setGroup(groupId);
            }
        }
        context.setMsg(matcher.replaceAll(""));

        return context;
    }

}
