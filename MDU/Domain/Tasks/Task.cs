using DDDSample1.Domain.Shared;
using System;


namespace DDDSample1.Domain.Tasks
{

    public class Task : Entity<TaskId>, IAggregateRoot
    {
        public new TaskId Id { get; private set; }

        public string UserID { get; private set; }

        public string UserEmail { get; private set; }

        public int StartX { get; private set; }
        
        public int StartY { get; private set; }
        
        public string StartFloorCode { get; private set; }
        
        public int EndX { get; private set; }
        
        public int EndY { get; private set; }
        
        public string EndFloorCode { get; private set; }

        public string Description { get; private set; }

        public string TaskType { get; private set; }

        public string RobotCode { get; private set; }

        public string TaskState { get; set; }

        public Task(TaskId taskId, string userID, string userEmail, int startX, int startY, string startFloorCode, int endX, int endY, string endFloorCode, string description, string taskType, string robotCode, string taskState)
        {
            this.Id = taskId;
            this.UserID = userID;
            this.UserEmail = userEmail;
            this.StartX = startX;
            this.StartY = startY;
            this.StartFloorCode = startFloorCode;
            this.EndX = endX;
            this.EndY = endY;
            this.EndFloorCode = endFloorCode;
            this.Description = description;
            this.TaskType = taskType;
            this.RobotCode = robotCode;
            this.TaskState = taskState;
        }

        private Task()
        {
            // Required for EF Core
        }


    }
}