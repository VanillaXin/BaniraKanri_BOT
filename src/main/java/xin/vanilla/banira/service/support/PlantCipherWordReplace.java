package xin.vanilla.banira.service.support;

import com.github.houbb.sensitive.word.api.IWordContext;
import com.github.houbb.sensitive.word.api.IWordReplace;
import com.github.houbb.sensitive.word.api.IWordResult;
import xin.vanilla.banira.util.PlantCipher;

/**
 * 敏感词替换为 PlantCipher 编码，便于事后解码核对出站内容。
 */
public class PlantCipherWordReplace implements IWordReplace {

    @Override
    public void replace(StringBuilder stringBuilder, String rawText, IWordResult wordResult, IWordContext wordContext) {
        String sensitiveWord = rawText.substring(wordResult.startIndex(), wordResult.endIndex());
        stringBuilder.append(PlantCipher.encode(sensitiveWord));
    }

}
