Feature: Default UI for Google Translate 

  Background:
    Given default UI
    Given I am in buffer "*Lorem Ipsum*"
    And the buffer is empty
    And I insert "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

  Scenario: Translate a word at point
    Given I go to word "dummy"
    When I translate word at point from "en" to "ru"
    Then I should see translation "манекен"

  Scenario: Translate an empty string
    When I translate " " from "en" to "ru"
    Then I should see message "Nothing to translate."

  Scenario: Translate a word using query translate
    When I translate "book" from "en" to "ru"
    Then I should see translation "книга"

  Scenario: Translate inputed word using language auto-detection and ido
    Given I set google-translate-enable-ido-completion to t
    When I translate "book" from "auto" to "ru"
    Then I should see translation "книга"

  Scenario: Translate inputed word using language auto-detection without ido
    Given I set google-translate-enable-ido-completion to nil
    When I translate "pen" from "auto" to "ru"
    Then I should see translation "ручка"

  Scenario: Translate a region
    When I go to word "It has"
    And I set the mark
    And I go to word "but"
    And I translate selected region from "en" to "ru"
    Then there is no region selected
    Then I should see translation "Это не только успешно пережил пять веков"

  Scenario: Translate a word at point using default source language
    Given I set google-translate-default-source-language to "en"
    Given I go to word "dummy"
    When I translate word at point to "ru"
    Then I should see translation "манекен"

  Scenario: Translate a word using query translate and default source language
    Given I set google-translate-default-source-language to "en"
    When I translate "book" to "ru"
    Then I should see translation "книга"
    
  Scenario: Translate a word at point using default target language
    Given I set google-translate-default-source-language to nil
    Given I set google-translate-default-target-language to "ru"
    Given I go to word "dummy"
    When I translate word at point from "en"
    Then I should see translation "манекен"

  Scenario: Translate a word using query translate and default target language
    Given I set google-translate-default-source-language to nil
    Given I set google-translate-default-target-language to "ru"
    When I translate "book" from "en"
    Then I should see translation "книга"
    
  Scenario: Translate a word at point using defaults source and target language
    Given I set google-translate-default-source-language to "en"
    Given I set google-translate-default-target-language to "ru"
    Given I go to word "dummy"
    When I translate word at point
    Then I should see translation "манекен"

  Scenario: Translate a word at point using defaults source and target language
    Given I set google-translate-default-source-language to "en"
    Given I set google-translate-default-target-language to "ru"
    Given I go to word "book"
    When I translate word at point
    Then I should see translation "книга"

  Scenario: Reverse translate word at point
    Given I set google-translate-default-source-language to "ru"
    Given I set google-translate-default-target-language to "en"
    Given I go to word "leap"
    When I reverse translate word at point
    Then I should see translation "прыжок"

  Scenario: Reverse translate a word
    Given I set google-translate-default-source-language to "ru"
    Given I set google-translate-default-target-language to "en"
    When I reverse translate "printer"
    Then I should see translation "принтер"

  Scenario: Translate a word at point using defaults source and target language
    Given I set google-translate-default-source-language to "ru"
    Given I set google-translate-default-target-language to "en"
    Given I go to word "book"
    When I reverse translate word at point
    Then I should see translation "книга"

  Scenario: Translate a word using query translate and defaults source and target language
    Given I set google-translate-default-source-language to "en"
    Given I set google-translate-default-target-language to "ru"
    When I translate "leap"
    Then I should see translation "прыжок"

  Scenario: Translate inputed word using language auto-detection
    Given I set google-translate-default-source-language to "auto"
    Given I set google-translate-default-target-language to "ru"
    When I translate "book"
    Then I should see translation "книга"

