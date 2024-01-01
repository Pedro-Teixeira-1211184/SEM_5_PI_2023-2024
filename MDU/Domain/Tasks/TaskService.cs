using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using System.Linq;
using System;

namespace DDDSample1.Domain.Tasks
{


    public class TaskService
    {


        private readonly IUnitOfWork _unitOfWork;
        private readonly ITaskRepository _repo;

        public TaskService(IUnitOfWork unitOfWork, ITaskRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }


        //Get All Tasks [Aysnc]
        public async Task<List<TaskDTO>> GetAllAsync()
        {

            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );
            return listDTO;
        }

        //Create a Task
        public async Task<TaskDTO> AddAsync(TaskDTO taskDTO)
        {
            // Generate a new TaskId (assuming it has logic for auto-generation)
            var taskId = TaskId.NewTaskId();

            // Create a new Task entity with the generated TaskId
            var task = new Task(taskId, taskDTO.UserEmail, taskDTO.StartX, taskDTO.StartY, taskDTO.StartFloorCode, taskDTO.EndX, taskDTO.EndY, taskDTO.EndFloorCode, taskDTO.Description, taskDTO.TaskType, taskDTO.RobotCode, taskDTO.TaskState);

            // Add the Task entity to the repository
            await this._repo.AddAsync(task);

            // Commit changes to the database
            await this._unitOfWork.CommitAsync();
            
            taskDTO.Id = taskId;

            // Return the DTO with the id
            return taskDTO;
        }

        //Get a Task by Id [Aysnc]
        public async Task<TaskDTO> GetByIdAsync(TaskId id)
        {

            var task = await this._repo.GetByIdAsync(id);

            TaskDTO taskDTO = new TaskDTO
            {
                Id = task.Id,
                UserEmail = task.UserEmail,
                StartX = task.StartX,
                StartY = task.StartY,
                StartFloorCode = task.StartFloorCode,
                EndX = task.EndX,
                EndY = task.EndY,
                EndFloorCode = task.EndFloorCode,
                Description = task.Description,
                TaskType = task.TaskType,
                RobotCode = task.RobotCode,
                TaskState = task.TaskState
            };

            return taskDTO;
        }

        //Return all tasks with state: PENDING
        public async Task<List<TaskDTO>> GetPendingTasksAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );

            List<TaskDTO> listPending = new List<TaskDTO>();

            foreach (TaskDTO task in listDTO)
            {
                if (task.TaskState == "PENDING")
                {
                    listPending.Add(task);
                }
            }

            return listPending;
        }

        //Return all tasks with state: ACCEPTED
        public async Task<List<TaskDTO>> GetAcceptedTasksAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );

            List<TaskDTO> listAccepted = new List<TaskDTO>();

            foreach (TaskDTO task in listDTO)
            {
                if (task.TaskState == "ACCEPTED")
                {
                    listAccepted.Add(task);
                }
            }

            return listAccepted;
        }

        //Return all tasks with state: DENIED
        public async Task<List<TaskDTO>> GetDeniedTasksAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );

            List<TaskDTO> listDenied = new List<TaskDTO>();

            foreach (TaskDTO task in listDTO)
            {
                if (task.TaskState == "DENIED")
                {
                    listDenied.Add(task);
                }
            }

            return listDenied;
        }
        
        //Return all tasks with state: DENIED or PENDING
        public async Task<List<TaskDTO>> GetNotApprovedTasksAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );

            List<TaskDTO> listNotApproved = new List<TaskDTO>();

            foreach (TaskDTO task in listDTO)
            {
                if (task.TaskState == "DENIED" || task.TaskState == "PENDING")
                {
                    listNotApproved.Add(task);
                }
            }

            return listNotApproved;
        }
        
        //Return all tasks
        public async Task<List<TaskDTO>> GetAllTasksAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<TaskDTO> listDTO = list.ConvertAll<TaskDTO>(
                task =>
                new TaskDTO
                {
                    Id = task.Id,
                    UserEmail = task.UserEmail,
                    StartX = task.StartX,
                    StartY = task.StartY,
                    StartFloorCode = task.StartFloorCode,
                    EndX = task.EndX,
                    EndY = task.EndY,
                    EndFloorCode = task.EndFloorCode,
                    Description = task.Description,
                    TaskType = task.TaskType,
                    RobotCode = task.RobotCode,
                    TaskState = task.TaskState
                }
            );

            return listDTO;
        }

        //Update Task State given Task and new TaskState
        public async Task<TaskDTO> UpdateTaskStateAsync(TaskDTO taskDTO, string newTaskState)
        {
            var task = await this._repo.GetByIdAsync(taskDTO.Id);

            task.TaskState = newTaskState;

            await this._unitOfWork.CommitAsync();

            return taskDTO;
        }

    }
}