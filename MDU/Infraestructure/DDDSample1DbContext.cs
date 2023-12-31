using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using DDDSample1.Domain.SystemUser;
using DDDSample1.Domain.Tasks;

namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : DbContext
    {
        public DbSet<User> Users { get; set; }

        public DbSet<Task> Tasks { get; set; }


        public DDDSample1DbContext(DbContextOptions options) : base(options)
        {

        }


        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {

            modelBuilder.Entity<User>()
                 .Property(u => u.Id)
                 .HasConversion(
                      v => v.Value,             // Converts UserId to its underlying type
                      v => new UserId(v));

            modelBuilder.Entity<User>()
                .HasIndex(u => u.NIF)
                .IsUnique();

            modelBuilder.Entity<User>()
                .HasIndex(u => u.Email)
                .IsUnique();

            modelBuilder.Entity<Task>()
                 .Property(t => t.Id)
                 .HasConversion(
                      v => v.Value,             // Converts TaskId to its underlying type
                      v => new TaskId(v));
        }

    }
}
