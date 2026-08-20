Feature: RSI plot

  Scenario: Plot RSI with overbought and oversold thresholds
    Given I have SPY data
    When I plot RSI chart
    Then I should see a ggplot
    And I should see no axis titles
    And RSI should be bounded by 0 and 100
    And I should see horizontal lines at 30 and 70
