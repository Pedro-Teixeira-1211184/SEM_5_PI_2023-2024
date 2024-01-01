using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Tasks;
using Microsoft.Extensions.Logging;
using System.Linq;

namespace DDDSample1.Controllers
{

    [Route("api/task")]
    [ApiController]
    public class TaskController : ControllerBase
    {
        private readonly TaskService _taskService;
        private readonly ILogger<TaskController> _logger;

        public TaskController(TaskService taskService, ILogger<TaskController> logger)
        {
            _taskService = taskService;
            _logger = logger;
        }

        //GET: api/task/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<TaskDTO>> GetById(string id)
        {
            var task = await _taskService.GetByIdAsync(new TaskId(id));

            if (task == null)
            {
                return NotFound();
            }

            return task;
        }

        //POST: Create a Task
        [HttpPost]
        public async Task<TaskDTO> CreateTask(TaskDTO createTaskDTO)
        {
            var createdTask = await _taskService.AddAsync(createTaskDTO);

            return createdTask;
        }

        //PATCH: /api/task/{id}/{state}
        [HttpPatch("{id}/{newTaskState}")]
        public async Task<ActionResult<TaskDTO>> UpdateTaskState(string id, string newTaskState)
        {
            var task = await _taskService.GetByIdAsync(new TaskId(id));

            if (task == null)
            {
                return NotFound();
            }

            var updatedTask = await _taskService.UpdateTaskStateAsync(task, newTaskState);

            return updatedTask;
        }
        
        //GET api/task
        [HttpGet]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllTasks()
        {
            var tasks = await _taskService.GetAllAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent(); // Return 204 No Content if there are no tasks
            }

            return tasks;
        }

        //GET api/task/accepted
        [HttpGet("accepted")]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllAcceptedTasks()
        {
            var tasks = await _taskService.GetAcceptedTasksAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent();
            }

            return tasks;
        }

        //GET api/task/pending
        [HttpGet("pending")]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllPendingTasks()
        {
            var tasks = await _taskService.GetPendingTasksAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent(); // Return 204 No Content if there are no tasks
            }

            return tasks;
        }
        
        //GET api/task/notapproved
        [HttpGet("notapproved")]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllNotApprovedTasks()
        {
            var tasks = await _taskService.GetNotApprovedTasksAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent(); // Return 204 No Content if there are no tasks
            }

            return tasks;
        }
    }
}