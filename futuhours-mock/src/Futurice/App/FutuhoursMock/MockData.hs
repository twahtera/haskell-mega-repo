{-# LANGUAGE OverloadedStrings     #-}
module Futurice.App.FutuhoursMock.MockData (
  projects,
  days
  ) where
import Futurice.App.FutuhoursMock.Types

internalProject = Project
      { _projectId=1
      , _projectName="Internal Work" 
      , _projectTasks=[
          Task { _taskId=1
               , _taskName="Things"
               , _taskLatestEntry=Entry { _entryDescription="Doing things"}
               , _taskClosed=False
               }
         ,Task { _taskId=2
               , _taskName="Stuff"
               , _taskLatestEntry=Entry { _entryDescription="Doing stuff"}
               , _taskClosed=False
               }
          ]
      , _projectClosed=False
      }

absenceProject = Project
      { _projectId=4
      , _projectName="Absences" 
      , _projectTasks=[
          Task { _taskId=6
               , _taskName="Balance leave"
               , _taskLatestEntry=Entry { _entryDescription="Balance leave"}
               , _taskClosed=False
               }
         ,Task { _taskId=7
               , _taskName="Unpaid holiday"
               , _taskLatestEntry=Entry { _entryDescription="Unpaid holiday"}
               , _taskClosed=False
               }
         ,Task { _taskId=8
               , _taskName="Sick leave"
               , _taskLatestEntry=Entry { _entryDescription="Sick leave"}
               , _taskClosed=False
               }
          ]
      , _projectClosed=False
      }

customerProject = Project
    { _projectId=2
    , _projectName="Actual customer work" 
    , _projectTasks=[
        Task { _taskId=3
             , _taskName="Development"
             , _taskLatestEntry=Entry { _entryDescription="Developing"}
             , _taskClosed=True
             }
       ,Task { _taskId=4
             , _taskName="On-Call"
             , _taskLatestEntry=Entry { _entryDescription="Long weekend :()"}
             , _taskClosed=True
             }
        ]
    , _projectClosed=False
    }

inactiveProject = Project
    { _projectId=3
    , _projectName="Not active project" 
    , _projectTasks=[
        Task { _taskId=5
             , _taskName="Work"
             , _taskLatestEntry=Entry { _entryDescription="Doing work"}
             , _taskClosed=False
             }
       ,Task { _taskId=6
             , _taskName="Design"
             , _taskLatestEntry=Entry { _entryDescription="Designing"}
             , _taskClosed=False
             }
        ]
    , _projectClosed=True
    }

projects = [
      internalProject
    , absenceProject
    , customerProject
    , inactiveProject]

days = [
     HoursDay {_dayHours=5
         , _dayEntries=[
              Entry { _entryId=1
                    , _entryProjectId=projectId internalProject
                    , _entryTaskId=0 -- TODO lens getter
                    , _entryDescription="Internal work"
                    , _entryHours=5
                    }
         }
    ,HoursDay { _dayHours=0.0
         , _dayHolidayname="Public holiday"
         }
    ,HoursDay { _dayHours=0.0 }
    ,HoursDay { _dayHours=0.0 }
    ,HoursDay { _dayHours=0.0
         , _dayClosed=True
         }
    ,HoursDay {_dayHours=7.5
        , _dayEntries=[
            Entry { _entryId=2
                  , _entryProjectId=projectId absenceProject
                  , _entryTaskId=0 -- TODO lens getter
                  , _entryDescription="TODO work" -- TODO lens getter
                  , _entryHours=7.5
                  }]
        }
    ,HoursDay {_dayHours=7.5
        , _dayClosed=True
        , _dayEntries=[
          Entry { _entryId=3
                , _entryProjectId=projectId absenceProject
                , _entryTaskId=0 -- TODO lens getter
                , _entryDescription="TODO work" -- TODO lens getter
                , _entryHours=7.5
                }]
        }
    ,HoursDay {_dayHours=10
        , _dayEntries=[
          Entry { _entryId=13
                , _entryProjectId=projectId customerProject
                , _entryTaskId=0 -- TODO lens getter
                , _entryDescription="TODO work" -- TODO lens getter
                , _entryHours=10
                }]
        }
    ,HoursDay {_dayHours=7.5
        , _dayEntries=[
            Entry { _entryId=4
                  , _entryProjectId=projectId absenceProject
                  , _entryTaskId=0 -- TODO lens getter
                  , _entryDescription="TODO work" -- TODO lens getter
                  , _entryHours=2.5
                  }
          , Entry { _entryId=5
                  , _entryProjectId=projectId customerProject
                  , _entryTaskId=0 -- TODO lens getter
                  , _entryDescription="Customer work"
                  , _entryHours=5.0
                  }]
        }
    ,HoursDay {_dayHours=9.0
        , _dayEntries=[
          Entry { _entryId=6
                , _entryProjectId=projectId inactiveProject
                , _entryTaskId=0 -- TODO lens getter
                , _entryDescription="TODO work" -- TODO lens getter
                , _entryHours=9.0
                , _entryClosed=True
                }]
        }
    ,HoursDay {_dayHours=7.5
        , _dayEntries=[
          Entry { _entryId=7
                , _entryProjectId=projectId inactiveProject
                , _entryTaskId=0 -- TODO lens getter
                , _entryDescription="TODO work" -- TODO lens getter
                , _entryHours=7.5
                , _entryClosed=True
                }]
        }
  , ]
