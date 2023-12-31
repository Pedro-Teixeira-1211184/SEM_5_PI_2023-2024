using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.SystemUser;
using Microsoft.Extensions.Logging;
using System.Linq;

namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]
    public class UserController : ControllerBase
    {
        private readonly UserService _userService;
        private readonly ILogger<UserController> _logger;

        public UserController(UserService userService, ILogger<UserController> logger)
        {
            _userService = userService;
            _logger = logger;
        }

        //GET: api/users
        [HttpGet]
        public async Task<ActionResult<IEnumerable<UserDTO>>> GetAll()
        {
            return await _userService.GetAllAsync();
        }

        // GET: api/users/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<UserDTO>> GetById(string id)
        {
            var user = await _userService.GetByIdAsync(new UserId(id));

            if (user == null)
            {
                return NotFound();
            }

            return user;
        }

        // POST: api/user
        [HttpPost]
        public async Task<ActionResult<UserDTO>> CreateUser(UserDTO createUserDTO)
        {
            var createdUser = await _userService.AddAsync(createUserDTO);

            return CreatedAtAction(nameof(GetById), new { id = createdUser.Id }, createdUser);
        }


        // GET: api/user
        [HttpGet("all")]
        public async Task<ActionResult<IEnumerable<UserDTO>>> GetAllUsers()
        {
            var users = await _userService.GetAllUsersAsync();

            if (users == null || !users.Any())
            {
                return NoContent(); // Return 204 No Content if there are no users
            }

            return Ok(users);
        }
        

        //api/user/{id}/delete
        [HttpDelete("{id}/delete")]
        public async Task<IActionResult> DeleteUser(string id)
        {
            _logger.LogInformation("Deleting user with id: " + id);

            var user = await _userService.GetByIdAsync(new UserId(id));

            if (user == null)
            {
                return NotFound();
            }

            try
            {
                await _userService.RemoveAsync(user.Id);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }

            return NoContent();
        }


    }
}