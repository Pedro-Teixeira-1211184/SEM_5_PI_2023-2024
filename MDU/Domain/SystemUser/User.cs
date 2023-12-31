using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.SystemUser
{
    public class User : Entity<UserId>, IAggregateRoot
    {
        public new UserId Id { get; private set; }
        public string FirstName { get; private set; }
        public string LastName { get; private set; }
        public string Email { get; private set; }
        public string NIF { get; private set; }
        public string Password { get; private set; }
        public string Role { get; private set; }

        // Updated constructor to take UserId parameter
        public User(UserId userId, string firstName, string lastName, string email, string password, string nif, string role)
        {
            Id = userId;
            FirstName = firstName;
            LastName = lastName;
            Email = email;
            Password = password;
            NIF = nif;
            Role = role;
        }
        // Private constructor required for EF Core
        private User()
        {
            // Required for EF Core
        }
    }
}
