package xin.vanilla.banira.plugin.chat.web;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class WebSearchServiceTest {

    @Test
    void shouldFilterLowValueSupportResults() {
        String html = """
                <html><body>
                  <li class="b_algo">
                    <h2><a href="https://support.google.com/youtube/">YouTube Help</a></h2>
                    <div class="b_caption"><p>Learn more about YouTube</p></div>
                  </li>
                  <li class="b_algo">
                    <h2><a href="https://example.com/profile">李いせん 简介</a></h2>
                    <div class="b_caption"><p>公开资料片段</p></div>
                  </li>
                </body></html>
                """;

        List<WebSearchService.SearchResult> results = WebSearchService.parseBingResults(html);

        Assertions.assertEquals(1, results.size());
        Assertions.assertEquals("李いせん 简介", results.getFirst().title());
    }

    @Test
    void shouldDropOpaqueBingRedirectResults() {
        String html = """
                <html><body>
                  <li class="b_algo">
                    <h2><a href="https://www.bing.com/ck/a?!&&p=abc&u=bad">广告结果</a></h2>
                    <div class="b_caption"><p>一堆跳转参数</p></div>
                  </li>
                  <li class="b_algo">
                    <h2><a href="https://example.com/meaning">梗百科：根母</a></h2>
                    <div class="b_caption"><p>网络用语解释</p></div>
                  </li>
                </body></html>
                """;

        List<WebSearchService.SearchResult> results = WebSearchService.parseBingResults(html);

        Assertions.assertEquals(1, results.size());
        Assertions.assertEquals("https://example.com/meaning", results.getFirst().url());
    }

    @Test
    void searchOutputShouldNotContainInternalPromptText() {
        String result = new WebSearchService().searchForTest("根母是什么", List.of(
                new WebSearchService.SearchResult("梗百科：根母", "https://example.com/meaning", "网络用语解释")
        ));

        Assertions.assertTrue(result.startsWith("查询：根母是什么"));
        Assertions.assertFalse(result.contains("请你基于"));
        Assertions.assertFalse(result.contains("不要原样"));
    }

    @Test
    void shouldFilterAdultSearchResults() {
        String html = """
                <html><body>
                  <li class="b_algo">
                    <h2><a href="https://freepussy.example/video">Puffy tits girl</a></h2>
                    <div class="b_caption"><p>porn xxx snippet</p></div>
                  </li>
                  <li class="b_algo">
                    <h2><a href="https://example.com/meaning">梗百科：根母</a></h2>
                    <div class="b_caption"><p>网络用语解释</p></div>
                  </li>
                </body></html>
                """;

        List<WebSearchService.SearchResult> results = WebSearchService.parseBingResults(html);

        Assertions.assertEquals(1, results.size());
        Assertions.assertEquals("梗百科：根母", results.getFirst().title());
    }

    @Test
    void shouldParseSearxngJsonResultsAndFilterNoise() {
        String json = """
                {
                  "results": [
                    {
                      "title": "YouTube Help",
                      "url": "https://support.google.com/youtube/",
                      "content": "Learn more about YouTube"
                    },
                    {
                      "title": "李いせん 简介",
                      "url": "https://example.com/profile",
                      "content": "<b>公开</b>&nbsp;资料片段"
                    }
                  ]
                }
                """;

        List<WebSearchService.SearchResult> results = WebSearchService.parseSearxngResults(json);

        Assertions.assertEquals(1, results.size());
        Assertions.assertEquals("李いせん 简介", results.getFirst().title());
        Assertions.assertEquals("https://example.com/profile", results.getFirst().url());
        Assertions.assertEquals("公开 资料片段", results.getFirst().snippet());
    }
}
