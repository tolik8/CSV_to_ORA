set define on

spool create.log
@create.sql

spool insert.log
@insert.sql

commit;

spool off

exit;
