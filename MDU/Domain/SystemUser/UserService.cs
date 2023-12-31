using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using System.Drawing;
using System.Linq;
using System;
using System.Text;
using Renci.SshNet.Security.Cryptography;

namespace DDDSample1.Domain.SystemUser
{

    public class UserService
    {

        private readonly IUnitOfWork _unitOfWork;
        private readonly IUserRepository _repo;


        public UserService(IUnitOfWork unitOfWork, IUserRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        //Get all Available Users [Aysnc]
        public async Task<List<UserDTO>> GetAllAsync()
        {

            var list = await this._repo.GetAllAsync();

            List<UserDTO> listDTO = list.ConvertAll<UserDTO>(
                user =>
                new UserDTO
                {
                    Id = user.Id,
                    Email = user.Email,
                    FirstName = user.FirstName,
                    LastName = user.LastName,
                    Password = user.Password,
                    NIF = user.NIF,
                    Role = user.Role
                }
            );
            return listDTO;
        }


        //Create an User
        public async Task<UserDTO> AddAsync(UserDTO userDTO)
        {
            // Generate a new UserId (assuming it has logic for auto-generation)
            var userId = UserId.NewUserId();

            // Create a new User entity with the generated UserId
            var user = new User(userId, userDTO.FirstName, userDTO.LastName, userDTO.Email, userDTO.Password, userDTO.NIF, userDTO.Role);

            // Add the User entity to the repository
            await this._repo.AddAsync(user);

            // Commit changes to the database
            await this._unitOfWork.CommitAsync();

            // Return the UserDTO with the generated UserId
            return new UserDTO
            {
                Id = user.Id,
                Email = user.Email,
                Password = user.Password,
                FirstName = user.FirstName,
                LastName = user.LastName,
                NIF = user.NIF,
                Role = user.Role
            };
        }

        public async Task<IEnumerable<UserDTO>> GetAllUsersAsync()
        {
            var users = await _repo.GetAllAsync(); // Assuming you have a method in your repository to retrieve all users

            return users.Select(user => new UserDTO
            {
                Id = user.Id,
                Email = user.Email,
                FirstName = user.FirstName,
                LastName = user.LastName,
                Password = user.Password, // Note: Consider whether you want to include the password in the DTO
                NIF = user.NIF,
                Role = user.Role
            });
        }



        public async Task<UserDTO> GetByIdAsync(UserId id)
        {
            var user = await this._repo.GetByIdAsync(id);

            if (user == null)
                return null;

            return new UserDTO
            {
                Id = user.Id,
                FirstName = user.FirstName,
                LastName = user.LastName,
                Email = user.Email,
                NIF = user.NIF,
                Password = user.Password, // Note: You might want to consider not returning sensitive data like passwords
                Role = user.Role

            };
        }

        public async Task RemoveAsync(UserId userId)
        {
            // Retrieve the user from the repository
            var user = await this._repo.GetByIdAsync(userId);

            // Remove the user from the repository
            this._repo.Remove(user);

            // Save the changes to the repository
            await this._unitOfWork.CommitAsync();
        }

        public bool ValidatePassword(UserDTO user, string enteredPassword)
        {
            // Implement your password validation logic here.
            // For simplicity, let's assume a plain comparison for now.
            return user.Password == enteredPassword;
        }

    }
}