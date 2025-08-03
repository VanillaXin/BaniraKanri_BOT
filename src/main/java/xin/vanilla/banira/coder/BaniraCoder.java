package xin.vanilla.banira.coder;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public abstract class BaniraCoder {
    protected final Long fromGroup;
    protected final Long fromUser;
    protected final Long toGroup;
    protected final Long toUser;
    protected final Long bot;

}
