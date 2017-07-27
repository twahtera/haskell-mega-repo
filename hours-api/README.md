# hours-api

## Architecture overview

`hours-ui` is good example of layered design!

- *The outer* is a HTTP API is defined in the
  [`API` module](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi/API.hs),
  with a help of outer facing types in
  [`Types` module](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi/Types.hs).

- *The center* is in
  [`Logic`](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi/Logic.hs)
  implementing functions resembling HTTP endpoints, but in abstract
  `MonadHours`, defined in [`Class` module](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi/Class.hs).
  `Class` defines it own types, not avoid coupling with outer http api,
  or inner PlanMill / Mock apis.

- *The inner* are concrete implementations of `MonadHours` type-class
    - [`HoursApi.Monad`](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi/Monad.hs)
   -  [`HoursMock.Monad`](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursMock/Monad.hs)

- *The main glue* are in
    - [`HoursApi`](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursApi.hs)
    - [`HoursMock`](https://github.com/futurice/haskell-mega-repo/blob/master/hours-api/src/Futurice/App/HoursMock.hs)

There are some subjective calls made. For example editing the timereport can be
done either via *edit* or *delete and create* internal methods. We do both:
*edit* if it's the task of the timereport isn't changed, and via *delete and
create* otherwise. This is done, because some properties of the task (e.g.
*billable/non-billable*) are copied into the timereport on creation. It's
easier to recreate the timereport then to try to update needed fields.

If we go past "should we always do edit as *delete and create*", the next
question is: **Should this logic be in `Logic` or `Monad`**. At the moment, I
don't see a good reason to prefer one over the other. Maybe later the `Logic`
will be different, based on the task.
