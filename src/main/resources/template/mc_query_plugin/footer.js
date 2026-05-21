/**
 * 统一页脚：左侧 功能名称·BaniraKanri，右侧 时间
 */
function ensureBaniraFooterStyle() {
    if (document.getElementById("banira-footer-style")) {
        return;
    }
    const style = document.createElement("style");
    style.id = "banira-footer-style";
    style.textContent = [
        "#banira-footer, .banira-footer-row {",
        "    display: flex;",
        "    justify-content: space-between;",
        "    align-items: center;",
        "    width: 100%;",
        "    gap: 12px;",
        "    box-sizing: border-box;",
        "}",
        ".banira-footer-brand {",
        "    flex: 0 1 auto;",
        "    text-align: left;",
        "}",
        ".banira-footer-time {",
        "    flex: 0 0 auto;",
        "    margin-left: auto;",
        "    text-align: right;",
        "    white-space: nowrap;",
        "}",
    ].join("\n");
    document.head.appendChild(style);
}

function formatBaniraFooterTime(date) {
    const now = date || new Date();
    const y = now.getFullYear();
    const M = String(now.getMonth() + 1).padStart(2, "0");
    const d = String(now.getDate()).padStart(2, "0");
    const h = String(now.getHours()).padStart(2, "0");
    const m = String(now.getMinutes()).padStart(2, "0");
    const s = String(now.getSeconds()).padStart(2, "0");
    return y + "-" + M + "-" + d + " " + h + ":" + m + ":" + s;
}

function formatBaniraFooterBrand(featureName) {
    return featureName + "·BaniraKanri";
}

function setBaniraFooter(elementOrId, featureName, date) {
    ensureBaniraFooterStyle();
    const el = typeof elementOrId === "string"
        ? document.getElementById(elementOrId)
        : elementOrId;
    if (!el) {
        return;
    }
    el.classList.add("banira-footer-row");
    el.innerHTML = '<span class="banira-footer-brand">' + formatBaniraFooterBrand(featureName)
        + '</span><span class="banira-footer-time">' + formatBaniraFooterTime(date) + "</span>";
}
