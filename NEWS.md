# rjira 0.1.0

* Перевёл функцию `jr_issue_search()` с устаревшего метода API [Currently being removed. Search for issues using JQL (POST)](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-post) на новый [Search for issues using JQL enhanced search (POST)](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-jql-post).

* Исправил проверку авторизации в `jr_check_auth()`: теперь корректно проверяются `JIRA_TOKEN`, `JIRA_USER` и `JIRA_BASE_API_URL`.
