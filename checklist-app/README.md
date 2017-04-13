# checklist-app

## Data mangling

### Select tribes in DB

```psql
select tribe from (select cmddata :: json #>> '{edit,tribe}' as tribe from checklist2.commands) as t group by t.tribe order by t.tribe;
```

### Update tribe

```psql
update checklist2.commands set cmddata = jsonb_set(cmddata :: jsonb, '{edit,tribe}' :: text[], jsonb('"New tribe"')) :: text where cmddata :: json #>> '{edit,tribe}' = 'oldtribe';
```
