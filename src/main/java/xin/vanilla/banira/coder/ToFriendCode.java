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
 * 转发至好友
 */
@Component
public class ToFriendCode implements BaniraCoder {

    private static final Pattern PATTERN = new RegUtils()
            .append(MSG_CODE_START)
            .groupNonIg("[tT][fF]|2[fF]|[tT]o[fF]riend|2[fF]riend")
            .append(ARG_SEPARATOR)
            .groupIgByName("friend", "\\d{5,10}")
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
            String friend = matcher.group("friend");
            long friendId = StringUtils.toLong(friend);
            if (BaniraUtils.isFriendIdValid(friendId)) {
                context.setTarget(friendId);
            }
        }
        context.setMsg(matcher.replaceAll(""));

        return context;
    }

}
