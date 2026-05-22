package xin.vanilla.banira.plugin.chat.web;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class WeatherServiceTest {

    @Test
    void shouldFormatWttrWeather() {
        String json = """
                {
                  "current_condition": [{
                    "temp_C": "26",
                    "FeelsLikeC": "27",
                    "humidity": "62",
                    "windspeedKmph": "8",
                    "weatherDesc": [{"value": "多云"}]
                  }],
                  "weather": [{
                    "mintempC": "22",
                    "maxtempC": "30"
                  }]
                }
                """;

        String result = WeatherService.formatWttrJson("成都", json);

        Assertions.assertTrue(result.contains("成都现在多云"));
        Assertions.assertTrue(result.contains("26℃"));
        Assertions.assertTrue(result.contains("最高30℃"));
    }
}
