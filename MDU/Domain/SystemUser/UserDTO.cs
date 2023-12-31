namespace DDDSample1.Domain.SystemUser {

    public class UserDTO
    {
        public UserId Id { get; set; }
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public string Email { get; set; }
        public string Password { get; set; }
        public string NIF { get; set; }
        public string Role { get; set; }

        // Default constructor for deserialization
        public UserDTO()
        {
        }

        // Constructor for creating a new UserDTO
        public UserDTO(UserId id, string email, string firstName, string lastName, string password, string nif, string role)
        {
            Id = id;
            Email = email;
            FirstName = firstName;
            LastName = lastName;
            Password = password;
            NIF = nif;
            Role = role;
        }
        
        public void setPassword(string pass){
            this.Password = pass;
        }

    }
}