library(RODBC)

# Note, this obnoxiously only works from Windows when we're authenticated to the Active Domain
# Kerberos authentication was a bridge too far

# Production remote DB
#dbhandle <- odbcDriverConnect('driver={SQL Server};server=sql2008\\psrcsql;database=shrp2c18final;trusted_connection=true')

# My local test DB
dbhandle <- odbcDriverConnect('driver={SQL Server};server=psrc3710\\SQLEXPRESS;database=shrp2c18final;trusted_connection=true')

# Sometimes it's handy to get a list of tables
# tabs <- sqlQuery(dbhandle, 'select * from information_schema.tables')


merged <- sqlQuery(dbhandle,
                   'SELECT R.ProjectID, R.QuestionID,
                      Q.Text AS "Question",
                      A.ID AS AnswerID, A.Text AS "Answer"
                    FROM Responses R

                    LEFT JOIN Answers A
                      ON R.AnswerID = A.ID

                    LEFT JOIN Questions Q
                      ON Q.ID = A.QuestionID

                    ORDER BY R.ProjectID, R.QuestionID;'
                   )

mergedfilt <- merged[! is.na(merged$AnswerID),]

odbcCloseAll()