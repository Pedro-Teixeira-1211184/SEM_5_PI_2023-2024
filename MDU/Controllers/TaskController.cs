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

    [Route("api/[controller]")]
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

        //GET: api/tasks/{id}
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
        public async Task<ActionResult<TaskDTO>> CreateTask(TaskDTO createTaskDTO)
        {
            var createdTask = await _taskService.AddAsync(createTaskDTO);

            return CreatedAtAction(nameof(GetById), new { id = createdTask.Id }, createdTask);
        }

        //PATCH: /api/tasks/{id}/updateState
        [HttpPatch("{id}/updateState")]
        public async Task<ActionResult<TaskDTO>> UpdateTaskState(string id, [FromBody] string newTaskState)
        {
            var task = await _taskService.GetByIdAsync(new TaskId(id));

            if (task == null)
            {
                return NotFound();
            }

            var updatedTask = await _taskService.UpdateTaskStateAsync(task, newTaskState);

            return updatedTask;
        }

        //GET api/acceptedTasks
        [HttpGet("acceptedTasks")]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllAcceptedTasks()
        {
            var tasks = await _taskService.GetAcceptedTasksAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent();
            }

            return tasks;
        }

        //GET api/pendingTasks
        [HttpGet("pendingTasks")]
        public async Task<ActionResult<IEnumerable<TaskDTO>>> GetAllPendingTasks()
        {
            var tasks = await _taskService.GetPendingTasksAsync();

            if (tasks == null || !tasks.Any())
            {
                return NoContent(); // Return 204 No Content if there are no tasks
            }

            return tasks;
        }
    }
}