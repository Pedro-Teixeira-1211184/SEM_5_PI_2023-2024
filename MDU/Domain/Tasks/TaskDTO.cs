using DDDSample1.Domain.SystemUser;

namespace DDDSample1.Domain.Tasks {

    public class TaskDTO {

        public TaskId Id { get; set; }
        public string UserEmail { get; set; }
        public int StartX { get; set; }
        public int StartY { get; set; }
        public string StartFloorCode { get; set; }
        public int EndX { get; set; }
        public int EndY { get; set; }
        public string EndFloorCode { get; set; }
        public string Description { get; set; }
        public string TaskType { get; set; }
        public string RobotCode { get; set; }
        public string TaskState { get; set; }

        // Default constructor for deserialization
        public TaskDTO()
        {
        }

        // Constructor for creating a new TaskDTO
        public TaskDTO(TaskId id, string userEmail, int startX, int startY, string startFloorCode, int endX, int endY, string endFloorCode, string description, string taskType, string robotCode, string taskState)
        {
            this.Id = id;
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


    }
}