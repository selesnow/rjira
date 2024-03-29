% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jr_get_issues_worklogs.R
\name{jr_get_issues_worklogs}
\alias{jr_get_issues_worklogs}
\title{Get issue worklogs}
\usage{
jr_get_issues_worklogs(
  issue_id_or_key,
  started_after = NULL,
  started_before = NULL,
  expand = "properties"
)
}
\arguments{
\item{issue_id_or_key}{The ID or key of the issue.}

\item{started_after}{The worklog start date and time.}

\item{started_before}{The worklog start date and time.}

\item{expand}{Use expand to include additional information about worklogs in the response. This parameter acceptsproperties, which returns worklog properties.}
}
\value{
tibble with worklogs
}
\description{
Returns worklogs for an issue, starting from the oldest worklog or from the worklog started on or after a date and time. For more details see next \href{https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-worklogs/#api-rest-api-3-issue-issueidorkey-worklog-get}{link}.
}
