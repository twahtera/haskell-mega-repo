-- create schema fumcarbon;
-- set schema 'fumcarbon';

create sequence fumcarbon.command_id_seq;

create table fumcarbon.commands (
  cid int not null default nextval('fumcarbon.command_id_seq') primary key,
  username text not null,
  updated timestamp with time zone not null default current_timestamp,
  cmddata text not null
);

-- create index commands_cid_idx ON fumcarbon.commands ((cmddata :: json ->> 'employecid'));
