Feature: Volume plot

  Scenario: Plot volume with up/down colors
    Given I have SPY data
    When I plot volume chart
    Then I should see a ggplot
    And I should see no axis titles
    And all volume values should be positive
    And I should see up and down colors
