Feature: Candlestick plot

  Scenario: Plot candlestick with default SMAs
    Given I have SPY data
    When I plot candlestick chart
    Then I should see a ggplot
    And I should see SMA_5 line
    And I should see SMA_20 line
    And I should see SMA_50 line
    And I should see SMA_200 line