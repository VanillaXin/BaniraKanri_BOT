package xin.vanilla.banira.plugin.keyword;

import com.github.houbb.pinyin.constant.enums.PinyinStyleEnum;
import com.github.houbb.pinyin.util.PinyinHelper;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.enums.EnumKeywordType;

/**
 * 拼音包含匹配
 */
@Component
public class PinyinMatchStrategy implements MatchStrategy {

    @Override
    public boolean match(String input, String keyword) {
        String inputPinyin = PinyinHelper.toPinyin(input, PinyinStyleEnum.NORMAL).trim();
        String keywordPinyin = PinyinHelper.toPinyin(keyword, PinyinStyleEnum.NORMAL).trim();
        return inputPinyin.contains(keywordPinyin);
    }

    @Override
    public EnumKeywordType getMatchType() {
        return EnumKeywordType.PINYIN;
    }

}
