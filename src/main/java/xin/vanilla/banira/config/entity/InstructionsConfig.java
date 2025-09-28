package xin.vanilla.banira.config.entity;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KanriInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.TimerInstructionsConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 指令配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class InstructionsConfig {

    /**
     * 前缀
     */
    private String prefix;
    /**
     * 基础指令
     */
    private BaseInstructionsConfig base;
    /**
     * 关键词指令
     */
    private KeyInstructionsConfig key;
    /**
     * 定时任务指令
     */
    private TimerInstructionsConfig timer;
    /**
     * 群管指令
     */
    private KanriInstructionsConfig kanri;

    /**
     * 获取表情图片
     */
    private List<String> imageFaceToImage;

    /**
     * 获取老婆
     */
    private List<String> wife;

    /**
     * 我的世界服务器查询
     */
    private List<String> mcQuery;

    /**
     * 戳一戳
     */
    private List<String> tap;

    /**
     * 存活检测
     */
    private List<String> alive;

    /**
     * 花言草语
     */
    private List<String> plant;

    /**
     * AI聊天
     */
    private List<String> aiChat;


    {
        this.prefix = "/bk";
        this.base = new BaseInstructionsConfig();
        this.key = new KeyInstructionsConfig();
        this.timer = new TimerInstructionsConfig();
        this.kanri = new KanriInstructionsConfig();
        this.imageFaceToImage = BaniraUtils.mutableListOf("获取表情", "获取图片", "获取表情图片", "getface", "getimage", "getimg");
        this.wife = BaniraUtils.mutableListOf("老婆", "wife");
        this.mcQuery = BaniraUtils.mutableListOf("我的世界", "麦块", "mc", "mcquery");
        this.tap = BaniraUtils.mutableListOf("戳一戳", "戳", "tap", "poke");
        this.alive = BaniraUtils.mutableListOf("存活", "活着", "在线", "活着?", "活着？", "online", "alive", "survival");
        this.plant = BaniraUtils.mutableListOf("plant", "tea", "花言草语", "胡言乱语", "喝茶", "品茶");
        this.aiChat = BaniraUtils.mutableListOf("aichat", "chat");
    }

}
