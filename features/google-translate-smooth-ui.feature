Feature: Smooth UI for Google Translate

  Background:
    Given 'smooth UI
    Given I am in buffer "*Lorem Ipsum*"
    And the buffer is empty
    And I insert "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

  Scenario: Translate a word at point
    Given I go to word "dummy"
    When I translate word at point from "en" to "ru"
    Then I should see translation "манекен"

  Scenario: Translate inputed word
    When I translate "book" from "en" to "de"
    Then I should see translation "buchen"

  Scenario: Translate inputed word using language auto-detection
    When I translate "book" from "auto" to "ru"
    Then I should see translation "книга"

  Scenario: Translate a word using current translation direction
    Given I set 'google-translate-translation-directions-alist to '(("en" . "ru") ("ru" . "en"))
    When I translate "dummy"
    Then I should see translation "манекен"
  
  Scenario: Translate a word using next translation direction
    Given I set 'google-translate-translation-directions-alist to '(("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "ручка" using '1 direction
    Then I should see translation "pen"

  Scenario: Translate a word using previous translation direction
    Given I set 'google-translate-translation-directions-alist to '(("en" . "ru") ("ru" . "uk") ("ru" . "en"))
    When I translate "ручка" using '-1 direction
    Then I should see translation "pen"

  Scenario: Last translation direction should be switched to the first one
    Given I set 'google-translate-translation-directions-alist to '(("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "pen" using '3 direction
    Then I should see translation "ручка"

  Scenario: Translate using language auto-detection
    Given I set 'google-translate-translation-directions-alist to '(("auto" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "car"
    Then I should see translation "автомобиль"

  Scenario: Translate a region
    Given I set 'google-translate-translation-directions-alist to '(("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I go to word "It has"
    And I set the mark
    And I go to word "but"
    And I translate ""
    Then there is no region selected
    Then I should see translation "Это не только успешно пережил пять веков"    



