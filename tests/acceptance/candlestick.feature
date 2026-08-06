Feature: Candlestick plot

  Scenario: Plot candlestick with default SMAs
    Given I have SPY data
    When I plot candlestick chart
    Then I should see a ggplot
