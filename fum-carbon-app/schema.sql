-- create schema fumcarbon;
-- set schema 'fumcarbon';

create sequence fumcarbon.command_id_seq;

create table fumcarbon.commands (
  command_id int not null default nextval('fumcarbon.command_id_seq') primary key,
  username text not null,  -- the initiator of command, used for audit
  created timestamp with time zone not null default current_timestamp,
  command text not null,   -- command name "create-user" or "add-group"
  payload jsonb not null   -- command data, the payload
);
