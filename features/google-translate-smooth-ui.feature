Feature: Smooth UI for Google Translate

  Background:
    Given smooth UI
    Given I am in buffer "*Lorem Ipsum*"
    And the buffer is empty
    And I insert "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

  Scenario: Translate a word at point
    Given I go to word "dummy"
    When I translate word at point from "en" to "ru"
    Then I should see translation "манекен"

  Scenario: Translate inputed word
    When I translate "book" from "en" to "ru"
    Then I should see translation "книга"

  Scenario: Translate inputed word using language auto-detection
    When I translate "book" from "auto" to "ru"
    Then I should see translation "книга"

  Scenario: Translate a word using current translation direction
    Given I set google-translate-translation-directions-alist to (("en" . "ru") ("ru" . "en"))
    When I translate "dummy"
    Then I should see translation "манекен"
  
  Scenario: Translate a word using next translation direction
    Given I set google-translate-translation-directions-alist to (("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "ручка" using 1 direction
    Then I should see translation "pen"

  Scenario: Translate a word using previous translation direction
    Given I set google-translate-translation-directions-alist to (("en" . "ru") ("ru" . "uk") ("ru" . "en"))
    When I translate "ручка" using -1 direction
    Then I should see translation "pen"

  Scenario: Last translation direction should be switched to the first one
    Given I set google-translate-translation-directions-alist to (("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "pen" using 3 direction
    Then I should see translation "ручка"

  Scenario: Translate using language auto-detection
    Given I set google-translate-translation-directions-alist to (("auto" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I translate "car"
    Then I should see translation "автомобиль"

  Scenario: Translate a region
    Given I set google-translate-translation-directions-alist to (("en" . "ru") ("ru" . "en") ("ru" . "uk"))
    When I go to word "It has"
    And I set the mark
    And I go to word "but"
    And I translate ""
    Then there is no region selected
    Then I should see translation "Он пережил не только пять веков,"

  Scenario: Suggestion when word is misspelled
    Given I set google-translate-translation-directions-alist to (("en" . "ru"))
    When I translate "sugest"
    Then I should see suggestion "suggest"

  Scenario: Linked suggestion: click on suggestion
    Given I set google-translate-translation-directions-alist to (("en" . "ru"))
    When I translate "sugest"
    Then I should see suggestion "suggest"
    And I press "TAB"
    And I press "TAB"
    And I press "TAB"
    And I press "RET"
    Then I should see translation "предлагать"

  Scenario: Translate a word emphasized with asterisks like *bold* such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "bold"
    When I translate word at point from "en" to "ru"
    Then I should see translation "смелый"

  Scenario: Translate a word emphasized with slashes like /italic/ such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "italic"
    When I translate word at point from "en" to "ru"
    Then I should see translation "курсив"

  Scenario: Translate a word emphasized with underscores like _underlined_ such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "underlined"
    When I translate word at point from "en" to "ru"
    Then I should see translation "подчеркнутый"

  Scenario: Translate a word emphasized with equals signs like =verbatim= such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "verbatim"
    When I translate word at point from "en" to "ru"
    Then I should see translation "дословно"

  Scenario: Translate a word emphasized with tildes like ~code~ such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "code"
    When I translate word at point from "en" to "ru"
    Then I should see translation "код"

  Scenario: Translate a word emphasized with pluses like ‘+strike-through+’ such as in Org mode
    Given I insert "You can make words *bold*, /italic/, _underlined_, =verbatim= and ~code~, and, if you must, ‘+strike-through+’."
    And I go to word "strike"
    When I translate word at point from "en" to "ru"
    Then I should see translation "забастовка"
